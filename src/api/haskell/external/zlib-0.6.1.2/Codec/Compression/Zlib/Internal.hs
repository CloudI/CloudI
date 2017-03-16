{-# LANGUAGE CPP, RankNTypes, DeriveDataTypeable, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2015 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Pure and IO stream based interfaces to lower level zlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Internal (

  -- * Pure interface
  compress,
  decompress,

  -- * Monadic incremental interface
  -- $incremental-compression

  -- ** Using incremental compression
  -- $using-incremental-compression

  CompressStream(..),
  compressST,
  compressIO,
  foldCompressStream,
  foldCompressStreamWithInput,

  -- ** Using incremental decompression
  -- $using-incremental-decompression

  DecompressStream(..),
  DecompressError(..),
  decompressST,
  decompressIO,
  foldDecompressStream,
  foldDecompressStreamWithInput,

  -- * The compression parameter types
  CompressParams(..),
  defaultCompressParams,
  DecompressParams(..),
  defaultDecompressParams,
  Stream.Format(..),
    Stream.gzipFormat,
    Stream.zlibFormat,
    Stream.rawFormat,
    Stream.gzipOrZlibFormat,
  Stream.CompressionLevel(..),
    Stream.defaultCompression,
    Stream.noCompression,
    Stream.bestSpeed,
    Stream.bestCompression,
    Stream.compressionLevel,
  Stream.Method(..),
    Stream.deflateMethod,
  Stream.WindowBits(..),
    Stream.defaultWindowBits,
    Stream.windowBits,
  Stream.MemoryLevel(..),
    Stream.defaultMemoryLevel,
    Stream.minMemoryLevel,
    Stream.maxMemoryLevel,
    Stream.memoryLevel,
  Stream.CompressionStrategy(..),
    Stream.defaultStrategy,
    Stream.filteredStrategy,
    Stream.huffmanOnlyStrategy,

  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (Exception, throw, assert)
import Control.Monad.ST.Lazy hiding (stToIO)
import Control.Monad.ST.Strict (stToIO)
#if __GLASGOW_HASKELL__ >= 702
import qualified Control.Monad.ST.Unsafe as Unsafe (unsafeIOToST)
#else
import qualified Control.Monad.ST.Strict as Unsafe (unsafeIOToST)
#endif
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import Data.Word (Word8)
import GHC.IO (noDuplicate)

import qualified Codec.Compression.Zlib.Stream as Stream
import Codec.Compression.Zlib.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
-- The 'compressBufferSize' is the size of the first output buffer containing
-- the compressed data. If you know an approximate upper bound on the size of
-- the compressed data then setting this parameter can save memory. The default
-- compression output buffer size is @16k@. If your extimate is wrong it does
-- not matter too much, the default buffer size will be used for the remaining
-- chunks.
--
data CompressParams = CompressParams {
  compressLevel       :: !Stream.CompressionLevel,
  compressMethod      :: !Stream.Method,
  compressWindowBits  :: !Stream.WindowBits,
  compressMemoryLevel :: !Stream.MemoryLevel,
  compressStrategy    :: !Stream.CompressionStrategy,
  compressBufferSize  :: !Int,
  compressDictionary  :: Maybe S.ByteString
} deriving Show

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
--
-- The 'decompressBufferSize' is the size of the first output buffer,
-- containing the uncompressed data. If you know an exact or approximate upper
-- bound on the size of the decompressed data then setting this parameter can
-- save memory. The default decompression output buffer size is @32k@. If your
-- extimate is wrong it does not matter too much, the default buffer size will
-- be used for the remaining chunks.
--
-- One particular use case for setting the 'decompressBufferSize' is if you
-- know the exact size of the decompressed data and want to produce a strict
-- 'Data.ByteString.ByteString'. The compression and deccompression functions
-- use lazy 'Data.ByteString.Lazy.ByteString's but if you set the
-- 'decompressBufferSize' correctly then you can generate a lazy
-- 'Data.ByteString.Lazy.ByteString' with exactly one chunk, which can be
-- converted to a strict 'Data.ByteString.ByteString' in @O(1)@ time using
-- @'Data.ByteString.concat' . 'Data.ByteString.Lazy.toChunks'@.
--
data DecompressParams = DecompressParams {
  decompressWindowBits :: !Stream.WindowBits,
  decompressBufferSize :: !Int,
  decompressDictionary :: Maybe S.ByteString,
  decompressAllMembers :: Bool
} deriving Show

-- | The default set of parameters for compression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressLevel       = Stream.defaultCompression,
  compressMethod      = Stream.deflateMethod,
  compressWindowBits  = Stream.defaultWindowBits,
  compressMemoryLevel = Stream.defaultMemoryLevel,
  compressStrategy    = Stream.defaultStrategy,
  compressBufferSize  = defaultCompressBufferSize,
  compressDictionary  = Nothing
}

-- | The default set of parameters for decompression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressWindowBits = Stream.defaultWindowBits,
  decompressBufferSize = defaultDecompressBufferSize,
  decompressDictionary = Nothing,
  decompressAllMembers = True
}

-- | The default chunk sizes for the output of compression and decompression
-- are 16k and 32k respectively (less a small accounting overhead).
--
defaultCompressBufferSize, defaultDecompressBufferSize :: Int
defaultCompressBufferSize   = 16 * 1024 - L.chunkOverhead
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead

-- | The unfolding of the decompression process, where you provide a sequence
-- of compressed data chunks as input and receive a sequence of uncompressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
--
-- To indicate the end of the input supply an empty input chunk. Note that
-- for 'gzipFormat' with the default 'decompressAllMembers' @True@ you will
-- have to do this, as the decompressor will look for any following members.
-- With 'decompressAllMembers' @False@ the decompressor knows when the data
-- ends and will produce 'DecompressStreamEnd' without you having to supply an
-- empty chunk to indicate the end of the input.
--
data DecompressStream m =

     DecompressInputRequired {
         decompressSupplyInput :: S.ByteString -> m (DecompressStream m)
       }

   | DecompressOutputAvailable {
         decompressOutput :: !S.ByteString,
         decompressNext   :: m (DecompressStream m)
       }

   -- | Includes any trailing unconsumed /input/ data.
   | DecompressStreamEnd {
         decompressUnconsumedInput :: S.ByteString
       }

   -- | An error code
   | DecompressStreamError {
         decompressStreamError :: DecompressError
       }

-- | The possible error cases when decompressing a stream.
--
-- This can be 'show'n to give a human readable error message.
--
data DecompressError =
     -- | The compressed data stream ended prematurely. This may happen if the
     -- input data stream was truncated.
     TruncatedInput

     -- | It is possible to do zlib compression with a custom dictionary. This
     -- allows slightly higher compression ratios for short files. However such
     -- compressed streams require the same dictionary when decompressing. This
     -- error is for when we encounter a compressed stream that needs a
     -- dictionary, and it's not provided.
   | DictionaryRequired

     -- | If the stream requires a dictionary and you provide one with the
     -- wrong 'DictionaryHash' then you will get this error.
   | DictionaryMismatch

     -- | If the compressed data stream is corrupted in any way then you will
     -- get this error, for example if the input data just isn't a compressed
     -- zlib data stream. In particular if the data checksum turns out to be
     -- wrong then you will get all the decompressed data but this error at the
     -- end, instead of the normal sucessful 'StreamEnd'.
   | DataFormatError String
  deriving (Eq, Typeable)

instance Show DecompressError where
  show TruncatedInput     = modprefix "premature end of compressed data stream"
  show DictionaryRequired = modprefix "compressed data stream requires custom dictionary"
  show DictionaryMismatch = modprefix "given dictionary does not match the expected one"
  show (DataFormatError detail) = modprefix ("compressed data stream format error (" ++ detail ++ ")")

modprefix :: ShowS
modprefix = ("Codec.Compression.Zlib: " ++)

instance Exception DecompressError

-- | A fold over the 'DecompressStream' in the given monad.
--
-- One way to look at this is that it runs the stream, using callback functions
-- for the four stream events.
--
foldDecompressStream :: Monad m
                     => ((S.ByteString -> m a) -> m a)
                     -> (S.ByteString -> m a -> m a)
                     -> (S.ByteString -> m a)
                     -> (DecompressError -> m a)
                     -> DecompressStream m -> m a
foldDecompressStream input output end err = fold
  where
    fold (DecompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (DecompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold (DecompressStreamEnd inchunk) = end inchunk
    fold (DecompressStreamError derr)  = err derr

-- | A variant on 'foldCompressStream' that is pure rather than operating in a
-- monad and where the input is provided by a lazy 'L.ByteString'. So we only
-- have to deal with the output, end and error parts, making it like a foldr on
-- a list of output chunks.
--
-- For example:
--
-- > toChunks = foldDecompressStreamWithInput (:) [] throw
--
foldDecompressStreamWithInput :: (S.ByteString -> a -> a)
                              -> (L.ByteString -> a)
                              -> (DecompressError -> a)
                              -> (forall s. DecompressStream (ST s))
                              -> L.ByteString
                              -> a
foldDecompressStreamWithInput chunk end err = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (DecompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (DecompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (DecompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold (DecompressStreamEnd inchunk) inchunks =
      return $ end (L.fromChunks (inchunk:inchunks))

    fold (DecompressStreamError derr) _ =
      return $ err derr


-- $incremental-compression
-- The pure 'compress' and 'decompress' functions are streaming in the sense
-- that they can produce output without demanding all input, however they need
-- the input data stream as a lazy 'L.ByteString'. Having the input data
-- stream as a lazy 'L.ByteString' often requires using lazy I\/O which is not
-- appropriate in all cicumstances.
--
-- For these cases an incremental interface is more appropriate. This interface
-- allows both incremental input and output. Chunks of input data are supplied
-- one by one (e.g. as they are obtained from an input source like a file or
-- network source). Output is also produced chunk by chunk.
--
-- The incremental input and output is managed via the 'CompressStream' and
-- 'DecompressStream' types. They represents the unfolding of the process of
-- compressing and decompressing. They operates in either the 'ST' or 'IO'
-- monads. They can be lifted into other incremental abstractions like pipes or
-- conduits, or they can be used directly in the following style.

-- $using-incremental-compression
--
-- In a loop:
--
--  * Inspect the status of the stream
--
--  * When it is 'CompressInputRequired' then you should call the action,
--    passing a chunk of input (or 'BS.empty' when no more input is available)
--    to get the next state of the stream and continue the loop.
--
--  * When it is 'CompressOutputAvailable' then do something with the given
--    chunk of output, and call the action to get the next state of the stream
--    and continue the loop.
--
--  * When it is 'CompressStreamEnd' then terminate the loop.
--
-- Note that you cannot stop as soon as you have no more input, you need to
-- carry on until all the output has been collected, i.e. until you get to
-- 'CompressStreamEnd'.
--
-- Here is an example where we get input from one file handle and send the
-- compressed output to another file handle.
--
-- > go :: Handle -> Handle -> CompressStream IO -> IO ()
-- > go inh outh (CompressInputRequired next) = do
-- >    inchunk <- BS.hGet inh 4096
-- >    go inh outh =<< next inchunk
-- > go inh outh (CompressOutputAvailable outchunk next) =
-- >    BS.hPut outh outchunk
-- >    go inh outh =<< next
-- > go _ _ CompressStreamEnd = return ()
--
-- The same can be achieved with 'foldCompressStream':
--
-- > foldCompressStream
-- >   (\next -> do inchunk <- BS.hGet inh 4096; next inchunk)
-- >   (\outchunk next -> do BS.hPut outh outchunk; next)
-- >   (return ())

-- $using-incremental-decompression
--
-- The use of 'DecompressStream' is very similar to 'CompressStream' but with
-- a few differences:
--
-- * There is the extra possibility of a 'DecompressStreamError'
--
-- * There can be extra trailing data after a compressed stream, and the
--   'DecompressStreamEnd' includes that.
--
-- Otherwise the same loop style applies, and there are fold functions.

-- | The unfolding of the compression process, where you provide a sequence
-- of uncompressed data chunks as input and receive a sequence of compressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
--
data CompressStream m =
     CompressInputRequired {
         compressSupplyInput :: S.ByteString -> m (CompressStream m)
       }

   | CompressOutputAvailable {
        compressOutput :: !S.ByteString,
        compressNext   :: m (CompressStream m)
      }

   | CompressStreamEnd

-- | A fold over the 'CompressStream' in the given monad.
--
-- One way to look at this is that it runs the stream, using callback functions
-- for the three stream events.
--
foldCompressStream :: Monad m
                   => ((S.ByteString -> m a) -> m a)
                   -> (S.ByteString -> m a -> m a)
                   -> m a
                   -> CompressStream m -> m a
foldCompressStream input output end = fold
  where
    fold (CompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (CompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold CompressStreamEnd =
      end

-- | A variant on 'foldCompressStream' that is pure rather than operating in a
-- monad and where the input is provided by a lazy 'L.ByteString'. So we only
-- have to deal with the output and end parts, making it just like a foldr on a
-- list of output chunks.
--
-- For example:
--
-- > toChunks = foldCompressStreamWithInput (:) []
--
foldCompressStreamWithInput :: (S.ByteString -> a -> a)
                            -> a
                            -> (forall s. CompressStream (ST s))
                            -> L.ByteString
                            -> a
foldCompressStreamWithInput chunk end = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (CompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (CompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (CompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold CompressStreamEnd _inchunks =
      return end


-- | Compress a data stream provided as a lazy 'L.ByteString'.
--
-- There are no expected error conditions. All input data streams are valid. It
-- is possible for unexpected errors to occur, such as running out of memory,
-- or finding the wrong version of the zlib C library, these are thrown as
-- exceptions.
--
compress   :: Stream.Format -> CompressParams -> L.ByteString -> L.ByteString

-- | Incremental compression in the 'ST' monad. Using 'ST' makes it possible
-- to write pure /lazy/ functions while making use of incremental compression.
--
compressST :: Stream.Format -> CompressParams -> CompressStream (ST s)

-- | Incremental compression in the 'IO' monad.
--
compressIO :: Stream.Format -> CompressParams -> CompressStream IO

compress   format params = foldCompressStreamWithInput
                             L.Chunk L.Empty
                             (compressStreamST format params)
compressST format params = compressStreamST  format params
compressIO format params = compressStreamIO  format params

compressStream :: Stream.Format -> CompressParams -> S.ByteString
               -> Stream (CompressStream Stream)
compressStream format (CompressParams compLevel method bits memLevel
                                strategy initChunkSize mdict) =

    \chunk -> do
      Stream.deflateInit format compLevel method bits memLevel strategy
      setDictionary mdict
      case chunk of
        _ | S.null chunk ->
          fillBuffers 20   --gzip header is 20 bytes, others even smaller

        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          fillBuffers initChunkSize

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int -> Stream (CompressStream Stream)
  fillBuffers outChunkSize = do
#ifdef DEBUG
    Stream.consistencyCheck
#endif

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ CompressInputRequired $ \chunk ->
           case chunk of
             _ | S.null chunk          -> drainBuffers True
             S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (CompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress
    -- and that therefore a BufferError is impossible

    let flush = if lastChunk then Stream.Finish else Stream.NoFlush
    status <- Stream.deflate flush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ CompressOutputAvailable chunk $ do
                    fillBuffers defaultCompressBufferSize
          else do fillBuffers defaultCompressBufferSize

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  Stream.finalise
                  return $ CompressOutputAvailable chunk (return CompressStreamEnd)
          else do Stream.finalise
                  return CompressStreamEnd

      Stream.Error code msg -> case code of
        Stream.BufferError  -> fail "BufferError should be impossible!"
        Stream.NeedDict _   -> fail "NeedDict is impossible!"
        _                   -> fail msg

  -- Set the custom dictionary, if we were provided with one
  -- and if the format supports it (zlib and raw, not gzip).
  setDictionary :: Maybe S.ByteString -> Stream ()
  setDictionary (Just dict)
    | Stream.formatSupportsDictionary format = do
        status <- Stream.deflateSetDictionary dict
        case status of
          Stream.Ok          -> return ()
          Stream.Error _ msg -> fail msg
          _                  -> fail "error when setting deflate dictionary"
  setDictionary _ = return ()


-- | Decompress a data stream provided as a lazy 'L.ByteString'.
--
-- It will throw an exception if any error is encountered in the input data.
-- If you need more control over error handling then use one the incremental
-- versions, 'decompressST' or 'decompressIO'.
--
decompress   :: Stream.Format -> DecompressParams -> L.ByteString -> L.ByteString

-- | Incremental decompression in the 'ST' monad. Using 'ST' makes it possible
-- to write pure /lazy/ functions while making use of incremental decompression.
--
decompressST :: Stream.Format -> DecompressParams -> DecompressStream (ST s)

-- | Incremental decompression in the 'IO' monad.
--
decompressIO :: Stream.Format -> DecompressParams -> DecompressStream IO

decompress   format params = foldDecompressStreamWithInput
                               L.Chunk (const L.Empty) throw
                               (decompressStreamST format params)
decompressST format params = decompressStreamST  format params
decompressIO format params = decompressStreamIO  format params


decompressStream :: Stream.Format -> DecompressParams
                 -> Bool -> S.ByteString
                 -> Stream (DecompressStream Stream)
decompressStream format (DecompressParams bits initChunkSize mdict allMembers)
                 resume =

    \chunk -> do
      inputBufferEmpty <- Stream.inputBufferEmpty
      outputBufferFull <- Stream.outputBufferFull
      assert inputBufferEmpty $
        if resume then assert (format == Stream.gzipFormat && allMembers) $
                       Stream.inflateReset
                  else assert outputBufferFull $
                       Stream.inflateInit format bits
      case chunk of
        _ | S.null chunk -> do
          -- special case to avoid demanding more input again
          -- always an error anyway
          when outputBufferFull $ do
            let outChunkSize = 1
            outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
            Stream.pushOutputBuffer outFPtr 0 outChunkSize
          drainBuffers True

        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          -- Normally we start with no output buffer (so counts as full) but
          -- if we're resuming then we'll usually still have output buffer
          -- space available
          assert (if not resume then outputBufferFull else True) $ return ()
          if outputBufferFull
            then fillBuffers initChunkSize
            else drainBuffers False

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int
              -> Stream (DecompressStream Stream)
  fillBuffers outChunkSize = do
#ifdef DEBUG
    Stream.consistencyCheck
#endif

    -- in this state there are two possabilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ DecompressInputRequired $ \chunk ->
           case chunk of
             _ | S.null chunk          -> drainBuffers True
             S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (DecompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least if a BufferError does occur that it must be due to a premature EOF

    status <- Stream.inflate Stream.NoFlush

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ DecompressOutputAvailable chunk $ do
                    fillBuffers defaultDecompressBufferSize
          else do fillBuffers defaultDecompressBufferSize

      Stream.StreamEnd      -> do
        -- The decompressor tells us we're done.
        -- Note that there may be input bytes still available if the stream is
        -- embeded in some other data stream, so we return any trailing data.
        inputBufferEmpty <- Stream.inputBufferEmpty
        if inputBufferEmpty
          then do finish (DecompressStreamEnd S.empty)
          else do (inFPtr, offset, length) <- Stream.popRemainingInputBuffer
                  let inchunk = S.PS inFPtr offset length
                  finish (DecompressStreamEnd inchunk)

      Stream.Error code msg -> case code of
        Stream.BufferError  -> finish (DecompressStreamError TruncatedInput)
        Stream.NeedDict adler -> do
          err <- setDictionary adler mdict
          case err of
            Just streamErr  -> finish streamErr
            Nothing         -> drainBuffers lastChunk
        Stream.DataError    -> finish (DecompressStreamError (DataFormatError msg))
        _                   -> fail msg

  -- Note even if we end with an error we still try to flush the last chunk if
  -- there is one. The user just has to decide what they want to trust.
  finish end = do
    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do (outFPtr, offset, length) <- Stream.popOutputBuffer
              return (DecompressOutputAvailable (S.PS outFPtr offset length) (return end))
      else return end

  setDictionary :: Stream.DictionaryHash -> Maybe S.ByteString
                -> Stream (Maybe (DecompressStream Stream))
  setDictionary _adler Nothing =
    return $ Just (DecompressStreamError DictionaryRequired)
  setDictionary _adler (Just dict) = do
    status <- Stream.inflateSetDictionary dict
    case status of
      Stream.Ok -> return Nothing
      Stream.Error Stream.DataError _   ->
        return $ Just (DecompressStreamError DictionaryMismatch)
      _ -> fail "error when setting inflate dictionary"


------------------------------------------------------------------------------

mkStateST :: ST s (Stream.State s)
mkStateIO :: IO (Stream.State RealWorld)
mkStateST = strictToLazyST Stream.mkState
mkStateIO = stToIO Stream.mkState

runStreamST :: Stream a -> Stream.State s -> ST s (a, Stream.State s)
runStreamIO :: Stream a -> Stream.State RealWorld -> IO (a, Stream.State RealWorld)
runStreamST strm zstate = strictToLazyST (Unsafe.unsafeIOToST noDuplicate >> Stream.runStream strm zstate)
runStreamIO strm zstate = stToIO (Stream.runStream strm zstate)

compressStreamIO :: Stream.Format -> CompressParams -> CompressStream IO
compressStreamIO format params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateIO
        let next = compressStream format params
        (strm', zstate') <- runStreamIO (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State RealWorld -> CompressStream IO
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamIO (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd

compressStreamST :: Stream.Format -> CompressParams -> CompressStream (ST s)
compressStreamST format params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateST
        let next = compressStream format params
        (strm', zstate') <- runStreamST (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State s -> CompressStream (ST s)
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamST (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd


decompressStreamIO :: Stream.Format -> DecompressParams -> DecompressStream IO
decompressStreamIO format params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateIO
        let next = decompressStream format params False
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate' (S.null chunk)
  where
    go :: DecompressStream Stream -> Stream.State RealWorld -> Bool
       -> IO (DecompressStream IO)
    go (DecompressInputRequired next) zstate !_ =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate' (S.null chunk)

    go (DecompressOutputAvailable chunk next) zstate !eof =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        go strm' zstate' eof

    go (DecompressStreamEnd unconsumed) zstate !eof
      | format == Stream.gzipFormat
      , decompressAllMembers params
      , not eof    = tryFollowingStream unconsumed zstate
      | otherwise  = finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate !_ = finaliseStreamError err zstate

    tryFollowingStream :: S.ByteString -> Stream.State RealWorld -> IO (DecompressStream IO)
    tryFollowingStream chunk zstate = case S.length chunk of
      0 -> return $ DecompressInputRequired $ \chunk' -> case S.length chunk' of
         0 -> finaliseStreamEnd S.empty zstate
         1 | S.head chunk' /= 0x1f
           -> finaliseStreamEnd chunk' zstate
         1 -> return $ DecompressInputRequired $ \chunk'' -> case S.length chunk'' of
            0 -> finaliseStreamEnd chunk' zstate
            _ -> checkHeaderSplit (S.head chunk') chunk'' zstate
         _    -> checkHeader chunk' zstate
      1 -> return $ DecompressInputRequired $ \chunk' -> case S.length chunk' of
         0    -> finaliseStreamEnd chunk zstate
         _    -> checkHeaderSplit (S.head chunk) chunk' zstate
      _       -> checkHeader chunk zstate

    checkHeaderSplit :: Word8 -> S.ByteString -> Stream.State RealWorld -> IO (DecompressStream IO)
    checkHeaderSplit 0x1f chunk zstate
      | S.head chunk == 0x8b = do
        let resume = decompressStream format params True (S.pack [0x1f, 0x8b])
        if S.length chunk > 1
          then do
            -- have to handle the remaining data in this chunk
            (DecompressInputRequired next, zstate') <- runStreamIO resume zstate
            (strm', zstate'') <- runStreamIO (next (S.tail chunk)) zstate'
            go strm' zstate'' False
          else do
            -- subtle special case when the chunk tail is empty
            -- yay for QC tests
            (strm, zstate') <- runStreamIO resume zstate
            go strm zstate' False
    checkHeaderSplit byte chunk zstate =
        finaliseStreamEnd (S.cons byte chunk) zstate

    checkHeader :: S.ByteString -> Stream.State RealWorld -> IO (DecompressStream IO)
    checkHeader chunk zstate
      | S.index chunk 0 == 0x1f
      , S.index chunk 1 == 0x8b = do
        let resume = decompressStream format params True chunk
        (strm', zstate') <- runStreamIO resume zstate
        go strm' zstate' False
    checkHeader chunk zstate = finaliseStreamEnd chunk zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamError err)


decompressStreamST :: Stream.Format -> DecompressParams -> DecompressStream (ST s)
decompressStreamST format params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateST
        let next = decompressStream format params False
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate' (S.null chunk)
  where
    go :: DecompressStream Stream -> Stream.State s -> Bool
       -> ST s (DecompressStream (ST s))
    go (DecompressInputRequired next) zstate !_ =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate' (S.null chunk)

    go (DecompressOutputAvailable chunk next) zstate !eof =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        go strm' zstate' eof

    go (DecompressStreamEnd unconsumed) zstate !eof
      | format == Stream.gzipFormat
      , decompressAllMembers params
      , not eof    = tryFollowingStream unconsumed zstate
      | otherwise  = finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate !_ = finaliseStreamError err zstate


    tryFollowingStream :: S.ByteString -> Stream.State s -> ST s (DecompressStream (ST s))
    tryFollowingStream chunk zstate = 
      case S.length chunk of
      0 -> return $ DecompressInputRequired $ \chunk' -> case S.length chunk' of
         0 -> finaliseStreamEnd S.empty zstate
         1 | S.head chunk' /= 0x1f
           -> finaliseStreamEnd chunk' zstate
         1 -> return $ DecompressInputRequired $ \chunk'' -> case S.length chunk'' of
            0 -> finaliseStreamEnd chunk' zstate
            _ -> checkHeaderSplit (S.head chunk') chunk'' zstate
         _    -> checkHeader chunk' zstate
      1 -> return $ DecompressInputRequired $ \chunk' -> case S.length chunk' of
         0    -> finaliseStreamEnd chunk zstate
         _    -> checkHeaderSplit (S.head chunk) chunk' zstate
      _       -> checkHeader chunk zstate

    checkHeaderSplit :: Word8 -> S.ByteString -> Stream.State s -> ST s (DecompressStream (ST s))
    checkHeaderSplit 0x1f chunk zstate
      | S.head chunk == 0x8b = do
        let resume = decompressStream format params True (S.pack [0x1f, 0x8b])
        if S.length chunk > 1
          then do
            -- have to handle the remaining data in this chunk
            (DecompressInputRequired next, zstate') <- runStreamST resume zstate
            (strm', zstate'') <- runStreamST (next (S.tail chunk)) zstate'
            go strm' zstate'' False
          else do
            -- subtle special case when the chunk tail is empty
            -- yay for QC tests
            (strm, zstate') <- runStreamST resume zstate
            go strm zstate' False
    checkHeaderSplit byte chunk zstate =
        finaliseStreamEnd (S.cons byte chunk) zstate

    checkHeader :: S.ByteString -> Stream.State s -> ST s (DecompressStream (ST s))
    checkHeader chunk zstate
      | S.index chunk 0 == 0x1f
      , S.index chunk 1 == 0x8b = do
        let resume = decompressStream format params True chunk
        (strm', zstate') <- runStreamST resume zstate
        go strm' zstate' False
    checkHeader chunk zstate = finaliseStreamEnd chunk zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamError err)
