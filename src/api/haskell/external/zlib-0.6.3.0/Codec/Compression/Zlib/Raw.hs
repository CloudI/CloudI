-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2014 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Compression and decompression of data streams in the raw deflate format.
--
-- The format is described in detail in RFC #1951:
-- <http://www.ietf.org/rfc/rfc1951.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Raw (
  
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

decompress :: ByteString -> ByteString
decompress = decompressWith defaultDecompressParams

decompressWith :: DecompressParams -> ByteString -> ByteString
decompressWith = Internal.decompress rawFormat

compress :: ByteString -> ByteString
compress = compressWith defaultCompressParams

compressWith :: CompressParams -> ByteString -> ByteString
compressWith = Internal.compress rawFormat
