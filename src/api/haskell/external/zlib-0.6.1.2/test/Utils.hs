{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS

import Test.QuickCheck

-------------------
-- QuickCheck Utils

maxStrSize :: Double
maxStrSize = 500

-- convert a QC size parameter into one for generating long lists,
-- growing inverse exponentially up to maxStrSize
strSize :: Int -> Int
strSize n = floor (maxStrSize * (1 - 2 ** (-fromIntegral n/100)))

instance Arbitrary BL.ByteString where
  arbitrary = sized $ \sz -> fmap BL.fromChunks $ listOf $ resize (sz `div` 2) arbitrary
  shrink = map BL.pack . shrink . BL.unpack

instance Arbitrary BS.ByteString where
  arbitrary = sized $ \sz -> resize (strSize sz) $ fmap BS.pack $ listOf $ arbitrary
  shrink = map BS.pack . shrink . BS.unpack


