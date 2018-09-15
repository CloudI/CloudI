{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test code and properties for "Codec.Compression.Zlib.Internal"
--
module Test.Codec.Compression.Zlib.Internal where

import Codec.Compression.Zlib.Internal
import Test.Codec.Compression.Zlib.Stream ()
import Test.QuickCheck

import Control.Monad (ap)


instance Arbitrary CompressParams where
  arbitrary = return CompressParams `ap` arbitrary `ap` arbitrary
                                    `ap` arbitrary `ap` arbitrary
                                    `ap` arbitrary `ap` arbitraryBufferSize
                                    `ap` return Nothing

arbitraryBufferSize :: Gen Int
arbitraryBufferSize = frequency $ [(10, return n) | n <- [1..1024]] ++
                                  [(20, return n) | n <- [1025..8192]] ++
                                  [(40, return n) | n <- [8193..131072]] ++
                                  [(1, return n) | n <- [131072..1048576]]


instance Arbitrary DecompressParams where
  arbitrary = return DecompressParams `ap` arbitrary
                                      `ap` arbitraryBufferSize
                                      `ap` return Nothing
                                      `ap` arbitrary

