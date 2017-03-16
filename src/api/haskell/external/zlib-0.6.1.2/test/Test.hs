{-# LANGUAGE CPP #-}

module Main where

import Codec.Compression.Zlib.Internal
import qualified Codec.Compression.Zlib     as Zlib
import qualified Codec.Compression.GZip     as GZip
import qualified Codec.Compression.Zlib.Raw as Raw

import Test.Codec.Compression.Zlib.Internal ()
import Test.Codec.Compression.Zlib.Stream ()

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils ()

import Control.Monad
import Control.Exception
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
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
      testProperty "prefixes of valid stream detected as truncated"  prop_truncated
    ],
    testGroup "unit tests" [
      testCase "simple gzip case"          test_simple_gzip,
      testCase "detect bad crc"            test_bad_crc,
      testCase "detect non-gzip"           test_non_gzip,
      testCase "detect custom dictionary"  test_custom_dict,
      testCase "dectect inflate with wrong dict"   test_wrong_dictionary,
      testCase "dectect inflate with right dict"   test_right_dictionary,
      testCase "handle trailing data"      test_trailing_data,
      testCase "multiple gzip members"     test_multiple_members,
      testCase "check small input chunks"  test_small_chunks,
      testCase "check empty input"         test_empty,
      testCase "check exception raised"    test_exception
    ]
  ]


prop_decompress_after_compress :: Format
                               -> CompressParams
                               -> DecompressParams
                               -> Property
prop_decompress_after_compress w cp dp =
   (w /= zlibFormat || decompressWindowBits dp >= compressWindowBits cp) &&
   -- Zlib decompression has been observed to fail with both compress and decompress
   -- window bits = 8. This seems to be contrary to the docs and to a quick reading
   -- of the zlib source code.
   (decompressWindowBits dp > compressWindowBits cp || decompressWindowBits dp > WindowBits 8) &&
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
    truncated = foldDecompressStreamWithInput (\_ r -> r) (\_ -> False)
                  (\err -> case err of TruncatedInput -> True; _ -> False)

    shortStrings = sized $ \sz -> resize (sz `div` 6) arbitrary


test_simple_gzip :: Assertion
test_simple_gzip =
  withSampleData "hello.gz" $ \hnd ->
    let decomp = decompressIO gzipFormat defaultDecompressParams
     in assertDecompressOk hnd decomp

test_bad_crc :: Assertion
test_bad_crc =
  withSampleData "bad-crc.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    msg <- assertDataFormatError err
    msg @?= "incorrect data check"

test_non_gzip :: Assertion
test_non_gzip = do
  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    msg <- assertDataFormatError err
    msg @?= "incorrect header check"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    msg <- assertDataFormatError err
    msg @?= "incorrect header check"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO rawFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    msg <- assertDataFormatError err
    msg @?= "invalid code lengths set"

  withSampleData "not-gzip" $ \hnd -> do
    let decomp = decompressIO gzipOrZlibFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    msg <- assertDataFormatError err
    msg @?= "incorrect header check"

test_custom_dict :: Assertion
test_custom_dict =
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams
    err <- assertDecompressError hnd decomp
    err @?= DictionaryRequired

test_wrong_dictionary :: Assertion
test_wrong_dictionary = do
  withSampleData "custom-dict.zlib" $ \hnd -> do
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary = -- wrong dict!
                                             Just (BS.pack [65,66,67])
                                         }

    err <- assertDecompressError hnd decomp
    err @?= DictionaryMismatch

test_right_dictionary :: Assertion
test_right_dictionary = do
  withSampleData "custom-dict.zlib" $ \hnd -> do
    dict <- readSampleData "custom-dict.zlib-dict"
    let decomp = decompressIO zlibFormat defaultDecompressParams {
                                           decompressDictionary =
                                             Just (toStrict dict)
                                         }
    assertDecompressOk hnd decomp

test_trailing_data :: Assertion
test_trailing_data =
  withSampleData "two-files.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams {
                   decompressAllMembers = False
                 }
    chunks <- assertDecompressOkChunks hnd decomp
    case chunks of
      [chunk] -> chunk @?= BS.Char8.pack "Test 1"
      _       -> assertFailure "expected single chunk"

test_multiple_members :: Assertion
test_multiple_members =
  withSampleData "two-files.gz" $ \hnd -> do
    let decomp = decompressIO gzipFormat defaultDecompressParams {
                   decompressAllMembers = True
                 }
    chunks <- assertDecompressOkChunks hnd decomp
    case chunks of
      [chunk1,
       chunk2] -> do chunk1 @?= BS.Char8.pack "Test 1"
                     chunk2 @?= BS.Char8.pack "Test 2"
      _       -> assertFailure "expected two chunks"

test_small_chunks :: Assertion
test_small_chunks = do
  uncompressedFile <- readSampleData "not-gzip"
  GZip.compress (smallChunks uncompressedFile) @?= GZip.compress uncompressedFile
  Zlib.compress (smallChunks uncompressedFile) @?= Zlib.compress uncompressedFile
  Raw.compress  (smallChunks uncompressedFile) @?= Raw.compress uncompressedFile

  GZip.decompress (smallChunks (GZip.compress uncompressedFile)) @?= uncompressedFile
  Zlib.decompress (smallChunks (Zlib.compress uncompressedFile)) @?= uncompressedFile
  Raw.decompress  (smallChunks (Raw.compress  uncompressedFile)) @?= uncompressedFile

  compressedFile   <- readSampleData "hello.gz"
  (GZip.decompress . smallChunks) compressedFile @?= GZip.decompress compressedFile

test_empty :: Assertion
test_empty = do
  -- Regression test to make sure we only ask for input once in the case of
  -- initially empty input. We previously asked for input twice before
  -- returning the error.
  let decomp = decompressIO zlibFormat defaultDecompressParams
  case decomp of
    DecompressInputRequired next -> do
      decomp' <- next BS.empty
      case decomp' of
        DecompressStreamError TruncatedInput -> return ()
        _ -> assertFailure "expected truncated error"

    _ -> assertFailure "expected input"

test_exception :: Assertion
test_exception =
 (do
    compressedFile <- readSampleData "bad-crc.gz"
    _ <- evaluate (BL.length (GZip.decompress compressedFile))
    assertFailure "expected exception")

  `catch` \err -> do
      msg <- assertDataFormatError err
      msg @?= "incorrect data check"

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

expected :: String -> String -> IO a
expected e g = assertFailure ("expected: " ++ e ++ "\nbut got: " ++ g)
            >> fail ""

assertDecompressOk :: Handle -> DecompressStream IO -> Assertion
assertDecompressOk hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> return ())
      (\err -> expected "decompress ok" (show err))

assertDecompressOkChunks :: Handle -> DecompressStream IO -> IO [BS.ByteString]
assertDecompressOkChunks hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\chunk -> liftM (chunk:))
      (\_ -> return [])
      (\err -> expected "decompress ok" (show err))

assertDecompressError :: Handle -> DecompressStream IO -> IO DecompressError
assertDecompressError hnd =
    foldDecompressStream
      (BS.hGet hnd 4000 >>=)
      (\_ r -> r)
      (\_ -> expected "StreamError" "StreamEnd")
      return

assertDataFormatError :: DecompressError -> IO String
assertDataFormatError (DataFormatError detail) = return detail
assertDataFormatError _                        = assertFailure "expected DataError"
                                              >> return ""
