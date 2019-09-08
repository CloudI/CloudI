-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2014 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Compression and decompression of data streams in the gzip format.
--
-- The format is described in detail in RFC #1952:
-- <http://www.ietf.org/rfc/rfc1952.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.GZip (

  -- | This module provides pure functions for compressing and decompressing
  -- streams of data in the gzip format and represented by lazy 'ByteString's.
  -- This makes it easy to use either in memory or with disk or network IO.
  --
  -- For example a simple gzip compression program is just:
  --
  -- > import qualified Data.ByteString.Lazy as ByteString
  -- > import qualified Codec.Compression.GZip as GZip
  -- >
  -- > main = ByteString.interact GZip.compress
  --
  -- Or you could lazily read in and decompress a @.gz@ file using:
  --
  -- > content <- fmap GZip.decompress (readFile file)
  --

  -- * Simple compression and decompression
  compress,
  decompress,

  -- * Extended api with control over compression parameters
  compressWith,
  decompressWith,

  CompressParams(..), defaultCompressParams,
  DecompressParams(..), defaultDecompressParams,

  -- ** The compression parameter types
  CompressionLevel(..),
    defaultCompression,
    noCompression,
    bestSpeed,
    bestCompression,
    compressionLevel,
  Method(..),
    deflateMethod,
  WindowBits(..),
    defaultWindowBits,
    windowBits,
  MemoryLevel(..),
    defaultMemoryLevel,
    minMemoryLevel,
    maxMemoryLevel,
    memoryLevel,
  CompressionStrategy(..),
    defaultStrategy,
    filteredStrategy,
    huffmanOnlyStrategy,

  ) where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Compression.Zlib.Internal as Internal
import Codec.Compression.Zlib.Internal hiding (compress, decompress)


-- | Decompress a stream of data in the gzip format.
--
-- There are a number of errors that can occur. In each case an exception will
-- be thrown. The possible error conditions are:
--
-- * if the stream does not start with a valid gzip header
--
-- * if the compressed stream is corrupted
--
-- * if the compressed stream ends permaturely
--
-- Note that the decompression is performed /lazily/. Errors in the data stream
-- may not be detected until the end of the stream is demanded (since it is
-- only at the end that the final checksum can be checked). If this is
-- important to you, you must make sure to consume the whole decompressed
-- stream before doing any IO action that depends on it.
--
decompress :: ByteString -> ByteString
decompress = decompressWith defaultDecompressParams


-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultCompressParams { ... }
--
decompressWith :: DecompressParams -> ByteString -> ByteString
decompressWith = Internal.decompress gzipFormat


-- | Compress a stream of data into the gzip format.
--
-- This uses the default compression parameters. In partiular it uses the
-- default compression level which favours a higher compression ratio over
-- compression speed, though it does not use the maximum compression level.
--
-- Use 'compressWith' to adjust the compression level or other compression
-- parameters.
--
compress :: ByteString -> ByteString
compress = compressWith defaultCompressParams


-- | Like 'compress' but with the ability to specify various compression
-- parameters. Typical usage:
--
-- > compressWith defaultCompressParams { ... }
--
-- In particular you can set the compression level:
--
-- > compressWith defaultCompressParams { compressLevel = BestCompression }
--
compressWith :: CompressParams -> ByteString -> ByteString
compressWith = Internal.compress gzipFormat
