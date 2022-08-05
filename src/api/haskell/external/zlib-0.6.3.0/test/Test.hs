{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Codec.Compression.Zlib.Internal
import qualified Codec.Compression.Zlib     as Zlib
import qualified Codec.Compression.GZip     as GZip
import qualified Codec.Compression.Zlib.Raw as Raw

import Test.Codec.Compression.Zlib.Internal ()
import Test.Codec.Compression.Zlib.Stream ()

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils ()

import Control.Monad
import Control.Monad.ST.Lazy (ST)
import Control.Exception
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
#if !MIN_VERSION_bytestring(0,11,0)
import qualified Data.ByteString.Internal as BS
#endif
import System.IO
#if !(MIN_VERSION_base(4,6,0))
import Prelude hiding (catch)
#endif


main :: IO ()
main = defaultMain $
  testGroup "zlib tests" [
    testGroup "property tests" [
      testProperty "decompress . compress = id (standard)"           prop_decompress_after_compress,
      testProperty "decompress . compress = id (Zlib -> GZipOrZLib)" prop_gziporzlib1,
      testProperty "decompress . compress = id (GZip -> GZipOrZlib)" prop_gziporzlib2,
      testProperty "concatenated gzip members"                       prop_gzip_concat,
      testProperty "multiple gzip members, boundaries (all 2-chunks)" prop_multiple_members_boundary2,
      testProperty "multiple gzip members, boundaries (all 3-chunks)" prop_multiple_members_boundary3,
      testProperty "prefixes of valid stream detected as truncated"  prop_truncated,
      testProperty "compress works with BSes with non-zero offset"   prop_compress_nonzero_bs_offset
    ],
    testGroup "unit tests" [
      testProperty "simple gzip case"          test_simple_gzip,
      testProperty "detect bad crc"            test_bad_crc,
      testProperty "detect non-gzip"           test_non_gzip,
      testProperty "detect custom dictionary"  test_custom_dict,
      testProperty "dectect inflate with wrong dict"   test_wrong_dictionary,
      testProperty "dectect inflate with right dict"   test_right_dictionary,
      testProperty "handle trailing data"      test_trailing_data,
      testProperty "multiple gzip members"     test_multiple_members,
      testProperty "check small input chunks"  test_small_chunks,
      testProperty "check empty input"         test_empty,
      testProperty "check exception raised"    test_exception
    ]
  ]


prop_decompress_after_compress :: Format
                               -> CompressParams
                               -> DecompressParams
                               -> Property
prop_decompress_after_compress w cp dp =
   (w /= zlibFormat || decompressWindowBits dp >= compressWindowBits cp) &&
   (decompressWindowBits dp > compressWindowBits cp) &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress w dp . compress w cp) id


prop_gziporzlib1 :: CompressParams
                 -> DecompressParams
                 -> Property
prop_gziporzlib1 cp dp =
   decompressWindowBits dp > compressWindowBits cp &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress zlibFormat cp) id


prop_gziporzlib2 :: CompressParams
                 -> DecompressParams
                 -> Property
prop_gziporzlib2 cp dp =
   decompressWindowBits dp >= compressWindowBits cp &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   liftM2 (==) (decompress gzipOrZlibFormat dp . compress gzipFormat cp) id

prop_gzip_concat :: CompressParams
                 -> DecompressParams
                 -> BL.ByteString
                 -> Property
prop_gzip_concat cp dp input =
   decompressWindowBits dp >= compressWindowBits cp &&
   decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
   let catComp = BL.concat (replicate 5 (compress gzipFormat cp input))
       compCat = compress gzipFormat cp (BL.concat (replicate 5 input))

    in decompress gzipFormat dp { decompressAllMembers = True } catComp
    == decompress gzipFormat dp { decompressAllMembers = True } compCat

prop_multiple_members_boundary2 :: Property
prop_multiple_members_boundary2 =
    forAll shortStrings $ \bs ->
      all (\c -> decomp c == BL.append bs bs)
          (twoChunkSplits (comp bs `BL.append` comp bs))
  where
    comp   = compress gzipFormat defaultCompressParams
    decomp = decompress gzipFormat defaultDecompressParams

    shortStrings = fmap BL.pack $ listOf arbitrary

prop_multiple_members_boundary3 :: Property
prop_multiple_members_boundary3 =
    forAll shortStrings $ \bs ->
      all (\c -> decomp c == BL.append bs bs)
          (threeChunkSplits (comp bs `BL.append` comp bs))
  where
    comp   = compress gzipFormat defaultCompressParams
    decomp = decompress gzipFormat defaultDecompressParams

    shortStrings = sized $ \sz -> resize (sz `div` 10) $
                   fmap BL.pack $ listOf arbitrary

prop_truncated :: Format -> Property
prop_truncated format =
   forAll shortStrings $ \bs ->
     all (truncated decomp)
         (init (BL.inits (comp bs)))
  -- All the initial prefixes of a valid compressed stream should be detected
  -- as truncated.
  where
    comp   = compress format defaultCompressParams
    decomp = decompressST format defaultDecompressParams
    truncated :: (forall s. DecompressStream (ST s)) -> BL.ByteString -> Bool
    truncated = foldDecompressStreamWithInput (\_ r -> r) (\_ -> False)
                  (\err -> case err of TruncatedInput -> True; _ -> False)

    shortStrings = sized $ \sz -> resize (sz `div` 6) arbitrary

prop_compress_nonzero_bs_offset :: BS.ByteString
                                -> Int
                                -> Property
prop_compress_nonzero_bs_offset original to_drop =
   to_drop > 0 &&
   BS.length original > to_drop ==>
   let input = BS.drop to_drop original
#if MIN_VERSION_bytestring(0,11,0)
       dropped = to_drop
#else
       (BS.PS _ptr dropped _length) = input
#endif
       input' = BL.pack $ BS.unpack input -- BL.fromStrict is only available since bytestring-0.10.4.0
       compressed = compress gzipFormat defaultCompressParams input'
       decompressed = decompress gzipFormat defaultDecompressParams compressed
   in  dropped == to_drop && decompressed == input'


test_simple_gzip :: Property
test_simple_gzip = ioProperty $
  withSampleData "hello.gz" $ \hnd ->
    let decomp = decompressIO gzipFormat defaultDecompressParams
     in assertDecompressOk hnd decomp

test_bad_crc :: Property
test_bad_crc = ioProperty $
  withSampleData "bad-crc.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    assertDecompressError hnd (assertDataFormatError "incorrect data check") decomp

test_non_gzip :: Property
test_non_gzip = conjoin
  [ ioProperty $ withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    assertDecompressError hnd (assertDataFormatError "incorrect header check") decomp

  , ioProperty $ withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    assertDecompressError hnd (assertDataFormatError "incorrect header check") decomp

  , ioProperty $ withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO rawFormat defaultDecompressParams
        checkError err = disjoin
          -- The majority of platforms throw this:
          [ assertDataFormatError "invalid code lengths set" err
          -- But on z15+ mainframes zlib employs CPU instruction DFLTCC,
          -- which returns error code with the same meaning.
          -- See http://publibfp.dhe.ibm.com/epubs/pdf/a227832c.pdf, page 26-37
          -- and https://github.com/haskell/zlib/issues/46
          , assertDataFormatError "Operation-Ending-Supplemental Code is 0x27" err
          ]
    assertDecompressError hnd checkError decomp

  , ioProperty $ withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipOrZlibFormat defaultDecompressParams
    assertDecompressError hnd (assertDataFormatError "incorrect header check") decomp
  ]

test_custom_dict :: Property
test_custom_dict = ioProperty $
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    assertDecompressError hnd (=== DictionaryRequired) decomp

test_wrong_dictionary :: Property
test_wrong_dictionary = ioProperty $
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary = -- wrong dict!
                                             Just (BS.pack [65,66,67])
                                         }

    assertDecompressError hnd (=== DictionaryMismatch) decomp

test_right_dictionary :: Property
test_right_dictionary = ioProperty $
  withSampleData "custom-dict.zlib" $ \hnd -> do
    dict <- readSampleData "custom-dict.zlib-dict"
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary =
                                             Just (toStrict dict)
                                         }
    assertDecompressOk hnd decomp

test_trailing_data :: Property
test_trailing_data = ioProperty $
  withSampleData "two-files.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams {
                   decompressAllMembers = False
                 }
        checkChunks chunks = case chunks of
          [chunk] -> chunk === BS.Char8.pack "Test 1"
          _       -> counterexample "expected single chunk" False
    assertDecompressOkChunks hnd checkChunks decomp


test_multiple_members :: Property
test_multiple_members = ioProperty $
  withSampleData "two-files.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams {
                   decompressAllMembers = True
                 }
        checkChunks chunks = case chunks of
          [chunk1, chunk2] ->
            chunk1 === BS.Char8.pack "Test 1" .&&. chunk2 === BS.Char8.pack "Test 2"
          _ -> counterexample "expected two chunks" False
    assertDecompressOkChunks hnd checkChunks decomp

test_small_chunks :: Property
test_small_chunks = ioProperty $ do
  uncompressedFile <- readSampleData "not-gzip"
  compressedFile   <- readSampleData "hello.gz"
  return $ conjoin
    [ GZip.compress (smallChunks uncompressedFile) === GZip.compress uncompressedFile
    , Zlib.compress (smallChunks uncompressedFile) === Zlib.compress uncompressedFile
    , Raw.compress  (smallChunks uncompressedFile) === Raw.compress uncompressedFile

    , GZip.decompress (smallChunks (GZip.compress uncompressedFile)) === uncompressedFile
    , Zlib.decompress (smallChunks (Zlib.compress uncompressedFile)) === uncompressedFile
    , Raw.decompress  (smallChunks (Raw.compress  uncompressedFile)) === uncompressedFile

    , (GZip.decompress . smallChunks) compressedFile === GZip.decompress compressedFile
    ]

test_empty :: Property
test_empty = ioProperty $ do
  -- Regression test to make sure we only ask for input once in the case of
  -- initially empty input. We previously asked for input twice before
  -- returning the error.
  let decomp = decompressIO zlibFormat defaultDecompressParams
  case decomp of
    DecompressInputRequired next -> do
      decomp' <- next BS.empty
      case decomp' of
        DecompressStreamError TruncatedInput -> return $ property True
        _ -> return $ counterexample "expected truncated error" False

    _ -> return $ counterexample "expected input" False

test_exception :: Property
test_exception = ioProperty $ do
  compressedFile <- readSampleData "bad-crc.gz"
  len <- try (evaluate (BL.length (GZip.decompress compressedFile)))
  return $ case len of
    Left err -> assertDataFormatError "incorrect data check" err
    Right{} -> counterexample "expected exception" False

toStrict :: BL.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,10,0)
toStrict = BL.toStrict
#else
toStrict = BS.concat . BL.toChunks
#endif

-----------------------
-- Chunk boundary utils

smallChunks :: BL.ByteString -> BL.ByteString
smallChunks = BL.fromChunks . map (\c -> BS.pack [c]) . BL.unpack

twoChunkSplits :: BL.ByteString -> [BL.ByteString]
twoChunkSplits bs = zipWith (\a b -> BL.fromChunks [a,b]) (BS.inits sbs) (BS.tails sbs)
  where
    sbs = toStrict bs

threeChunkSplits :: BL.ByteString -> [BL.ByteString]
threeChunkSplits bs =
    [ BL.fromChunks [a,b,c]
    | (a,x) <- zip (BS.inits sbs) (BS.tails sbs)
    , (b,c) <- zip (BS.inits x) (BS.tails x) ]
  where
    sbs = toStrict bs

--------------
-- HUnit Utils

readSampleData :: FilePath -> IO BL.ByteString
readSampleData file = BL.readFile ("test/data/" ++ file)

withSampleData :: FilePath -> (Handle -> IO a) -> IO a
withSampleData file = withFile ("test/data/" ++ file) ReadMode

expected :: String -> String -> Property
expected e g = counterexample ("expected: " ++ e ++ "\nbut got: " ++ g) False

assertDecompressOk :: Handle -> DecompressStream IO -> IO Property
assertDecompressOk hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> return $ property True)
      (\err -> return $ expected "decompress ok" (show err))

assertDecompressOkChunks :: Handle -> ([BS.ByteString] -> Property) -> DecompressStream IO -> IO Property
assertDecompressOkChunks hnd callback = fmap (either id callback) .
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\chunk -> liftM (liftM (chunk:)))
      (\_ -> return $ Right [])
      (\err -> return $ Left $ expected "decompress ok" (show err))

assertDecompressError :: Handle -> (DecompressError -> Property) -> DecompressStream IO -> IO Property
assertDecompressError hnd callback =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> return $ expected "StreamError" "StreamEnd")
      (return . callback)

assertDataFormatError :: String -> DecompressError -> Property
assertDataFormatError expect (DataFormatError actual) = expect === actual
assertDataFormatError _ _ = counterexample "expected DataError" False
