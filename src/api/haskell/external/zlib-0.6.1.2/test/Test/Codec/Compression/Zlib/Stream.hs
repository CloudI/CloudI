{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test code and properties for "Codec.Compression.Zlib.Stream"
--
module Test.Codec.Compression.Zlib.Stream where

import Codec.Compression.Zlib.Internal
import Test.QuickCheck


instance Arbitrary Format where
  -- GZipOrZlib omitted since it's not symmetric
  arbitrary = elements [gzipFormat, zlibFormat, rawFormat]


instance Arbitrary Method where
   arbitrary = return deflateMethod


instance Arbitrary CompressionLevel where
  arbitrary = elements $ [defaultCompression, noCompression,
                          bestCompression, bestSpeed]
                      ++ map compressionLevel [1..9]


instance Arbitrary WindowBits where
  arbitrary = elements $ defaultWindowBits:map windowBits [8..15]


instance Arbitrary MemoryLevel where
  arbitrary = elements $ [defaultMemoryLevel, minMemoryLevel, maxMemoryLevel]
                      ++ [memoryLevel n | n <- [1..9]]



instance Arbitrary CompressionStrategy where
  arbitrary = elements $ [defaultStrategy, filteredStrategy, huffmanOnlyStrategy]
                   -- These are disabled by default in the package
                   -- as they are only available with zlib >=1.2
                   -- ++ [RLE, Fixed]
