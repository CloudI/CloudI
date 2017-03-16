module Main where

import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as GZip

main = B.interact GZip.decompress
