{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  MIT License

  Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

 -}

-- | Erlang External Term Format Encoding/Decoding

module Foreign.Erlang
    ( OtpErlangTerm(..)
    , Error(..)
    , Result
    , binaryToTerm
    , termToBinary
    ) where

import Prelude hiding (length,tail,(<>))
import Data.Bits ((.&.))
import Control.Monad (replicateM)
import qualified Data.Monoid as Monoid
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as DataInt
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Word as Word
import qualified Codec.Compression.Zlib as Zlib
import qualified Foreign.Erlang.Pid as E
import qualified Foreign.Erlang.Port as E
import qualified Foreign.Erlang.Reference as E
import qualified Foreign.Erlang.Function as E
type Get = Get.Get
type Builder = Builder.Builder
type ByteString = ByteString.ByteString
type LazyByteString = LazyByteString.ByteString
type Int32 = DataInt.Int32
type Map = Map.Map
type Word8 = Word.Word8
type Word16 = Word.Word16
type Word32 = Word.Word32
type Pid = E.Pid
type Port = E.Port
type Reference = E.Reference
type Function = E.Function

-- tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
tagVersion :: Word8
tagVersion = 131
tagCompressedZlib :: Word8
tagCompressedZlib = 80
tagNewFloatExt :: Word8
tagNewFloatExt = 70
tagBitBinaryExt :: Word8
tagBitBinaryExt = 77
tagAtomCacheRef :: Word8
tagAtomCacheRef = 78
tagNewPidExt :: Word8
tagNewPidExt = 88
tagNewPortExt :: Word8
tagNewPortExt = 89
tagNewerReferenceExt :: Word8
tagNewerReferenceExt = 90
tagSmallIntegerExt :: Word8
tagSmallIntegerExt = 97
tagIntegerExt :: Word8
tagIntegerExt = 98
tagFloatExt :: Word8
tagFloatExt = 99
tagAtomExt :: Word8
tagAtomExt = 100
tagReferenceExt :: Word8
tagReferenceExt = 101
tagPortExt :: Word8
tagPortExt = 102
tagPidExt :: Word8
tagPidExt = 103
tagSmallTupleExt :: Word8
tagSmallTupleExt = 104
tagLargeTupleExt :: Word8
tagLargeTupleExt = 105
tagNilExt :: Word8
tagNilExt = 106
tagStringExt :: Word8
tagStringExt = 107
tagListExt :: Word8
tagListExt = 108
tagBinaryExt :: Word8
tagBinaryExt = 109
tagSmallBigExt :: Word8
tagSmallBigExt = 110
tagLargeBigExt :: Word8
tagLargeBigExt = 111
tagNewFunExt :: Word8
tagNewFunExt = 112
tagExportExt :: Word8
tagExportExt = 113
tagNewReferenceExt :: Word8
tagNewReferenceExt = 114
tagSmallAtomExt :: Word8
tagSmallAtomExt = 115
tagMapExt :: Word8
tagMapExt = 116
tagFunExt :: Word8
tagFunExt = 117
tagAtomUtf8Ext :: Word8
tagAtomUtf8Ext = 118
tagSmallAtomUtf8Ext :: Word8
tagSmallAtomUtf8Ext = 119
tagV4PortExt :: Word8
tagV4PortExt = 120
tagLocalExt :: Word8
tagLocalExt = 121

data OtpErlangTerm =
      OtpErlangInteger Int
    | OtpErlangIntegerBig Integer
    | OtpErlangFloat Double
    | OtpErlangAtom ByteString
    | OtpErlangAtomUTF8 ByteString
    | OtpErlangAtomCacheRef Int
    | OtpErlangAtomBool Bool
    | OtpErlangString ByteString
    | OtpErlangBinary ByteString
    | OtpErlangBinaryBits (ByteString, Int)
    | OtpErlangList [OtpErlangTerm]
    | OtpErlangListImproper [OtpErlangTerm]
    | OtpErlangTuple [OtpErlangTerm]
    | OtpErlangMap (Map OtpErlangTerm OtpErlangTerm)
    | OtpErlangPid Pid
    | OtpErlangPort Port
    | OtpErlangReference Reference
    | OtpErlangFunction Function
    deriving (Ord, Eq, Show)

data Error =
      InputError String
    | OutputError String
    | ParseError String
    deriving (Eq, Show)

type Result a = Either Error a

ok :: a -> Result a
ok value = Right value

errorType :: Error -> Result a
errorType value = Left value

getUnsignedInt8 :: Word8 -> Int
getUnsignedInt8 value = fromIntegral value

getUnsignedInt16 :: Word16 -> Int
getUnsignedInt16 value = fromIntegral value

getUnsignedInt32 :: Word32 -> Int
getUnsignedInt32 value = fromIntegral value

getSignedInt32 :: Word32 -> Int
getSignedInt32 value = fromIntegral (fromIntegral value :: Int32)

getUnsignedInt8or32 :: Bool -> Get Int
getUnsignedInt8or32 True = do
    value <- Get.getWord8
    return $ getUnsignedInt8 value
getUnsignedInt8or32 False = do
    value <- Get.getWord32be
    return $ getUnsignedInt32 value

boolTrue :: ByteString
boolTrue = Char8.pack "true"
boolFalse :: ByteString
boolFalse = Char8.pack "false"

infixr 4 <>
(<>) :: Monoid.Monoid m => m -> m -> m
(<>) = Monoid.mappend

-- | Decode Erlang terms within binary data into Haskell types
binaryToTerm :: LazyByteString -> Result OtpErlangTerm
binaryToTerm binary =
    let size = LazyByteString.length binary in
    if size <= 1 then
        errorType $ ParseError "null input"
    else if LazyByteString.head binary /= tagVersion then
        errorType $ ParseError "invalid version"
    else
        case Get.runGetOrFail binaryToTerms (LazyByteString.tail binary) of
            Left (_, _, err) ->
                errorType $ ParseError err
            Right (_, _, term) ->
                ok term

-- | Encode Haskell types into Erlang terms in binary data
termToBinary :: OtpErlangTerm -> Int -> Result LazyByteString
termToBinary term compressed
    | compressed < (-1) || compressed > 9 =
        errorType $ InputError "compressed in [-1..9]"
    | otherwise =
        case termsToBinary term of
            Left err ->
                errorType err
            Right dataUncompressed ->
                if compressed == (-1) then
                    ok $ LazyByteString.cons tagVersion dataUncompressed
                else
                    let sizeUncompressed =
                            LazyByteString.length dataUncompressed
                        params = Zlib.defaultCompressParams {
                            Zlib.compressLevel =
                            Zlib.CompressionLevel compressed }
                        dataCompressed =
                            Zlib.compressWith params dataUncompressed in
                    if sizeUncompressed > 4294967295 then
                        errorType $ OutputError "uint32 overflow"
                    else
                        ok $ Builder.toLazyByteString $
                            Builder.word8 tagVersion <>
                            Builder.word8 tagCompressedZlib <>
                            Builder.word32BE (fromIntegral sizeUncompressed) <>
                            Builder.lazyByteString dataCompressed

binaryToTerms :: Get OtpErlangTerm
binaryToTerms = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagNewFloatExt -> do
            value <- Get.getDoublebe
            return $ OtpErlangFloat value
        | tag == tagBitBinaryExt -> do
            j <- Get.getWord32be
            bits <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt32 j
            return $ OtpErlangBinaryBits (value, getUnsignedInt8 bits)
        | tag == tagAtomCacheRef -> do
            value <- Get.getWord8
            return $ OtpErlangAtomCacheRef $ getUnsignedInt8 value
        | tag == tagSmallIntegerExt -> do
            value <- Get.getWord8
            return $ OtpErlangInteger $ getUnsignedInt8 value
        | tag == tagIntegerExt -> do
            value <- Get.getWord32be
            return $ OtpErlangInteger $ getSignedInt32 value
        | tag == tagFloatExt -> do
            str <- Get.getByteString 31
            let value = Char8.unpack $ Char8.takeWhile (\c -> c /= '\0') str
            return $ OtpErlangFloat (read value :: Double)
        | tag == tagV4PortExt || tag == tagNewPortExt ||
          tag == tagReferenceExt || tag == tagPortExt -> do
            (nodeTag, node) <- binaryToAtom
            let idSize = if tag == tagV4PortExt then 8 else 4
            eid <- Get.getByteString idSize
            let creationSize =
                    if tag == tagV4PortExt || tag == tagNewPortExt then
                        4
                    else
                        1
            creation <- Get.getByteString creationSize
            if tag == tagReferenceExt then
                return $ OtpErlangReference $ E.Reference
                    nodeTag node eid creation
            else
                -- tag == tagV4PortExt || tag == tagNewPortExt ||
                -- tag == tagPortExt
                return $ OtpErlangPort $ E.Port
                    nodeTag node eid creation
        | tag == tagNewPidExt || tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            eid <- Get.getByteString 4
            serial <- Get.getByteString 4
            let creationSize = if tag == tagNewPidExt then 4 else 1
            creation <- Get.getByteString creationSize
            return $ OtpErlangPid $ E.Pid
                nodeTag node eid serial creation
        | tag == tagSmallTupleExt || tag == tagLargeTupleExt -> do
            length <- getUnsignedInt8or32 $ tag == tagSmallTupleExt
            value <- binaryToTermSequence length
            return $ OtpErlangTuple value
        | tag == tagNilExt -> do
            return $ OtpErlangList []
        | tag == tagStringExt -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            return $ OtpErlangString value
        | tag == tagListExt -> do
            length <- Get.getWord32be
            tmp <- binaryToTermSequence $ getUnsignedInt32 length
            tail <- binaryToTerms
            if tail == OtpErlangList [] then
                return $ OtpErlangList tmp
            else
                return $ OtpErlangListImproper $ tmp ++ [tail]
        | tag == tagBinaryExt -> do
            j <- Get.getWord32be
            value <- Get.getByteString $ getUnsignedInt32 j
            return $ OtpErlangBinary value
        | tag == tagSmallBigExt || tag == tagLargeBigExt -> do
            j <- getUnsignedInt8or32 (tag == tagSmallBigExt)
            sign <- Get.getWord8
            digits <- replicateM j Get.getWord8
            let f = (\d -> \b -> b * 256 + (fromIntegral . getUnsignedInt8) d)
                value = List.foldr f (0 :: Integer) digits
            if sign == 1 then
                return $ OtpErlangIntegerBig $ (-1) * value
            else
                return $ OtpErlangIntegerBig value
        | tag == tagNewFunExt -> do
            length <- Get.getWord32be
            value <- Get.getByteString $ getUnsignedInt32 length
            return $ OtpErlangFunction $ E.Function
                tag value
        | tag == tagExportExt -> do
            length <- Get.lookAhead $ binaryToExportSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ E.Function
                tag value
        | tag == tagNewerReferenceExt || tag == tagNewReferenceExt -> do
            j <- Get.getWord16be
            (nodeTag, node) <- binaryToAtom
            let creationSize = if tag == tagNewerReferenceExt then 4 else 1
            creation <- Get.getByteString creationSize
            eid <- Get.getByteString $ (getUnsignedInt16 j) * 4
            return $ OtpErlangReference $ E.Reference
                nodeTag node eid creation
        | tag == tagMapExt -> do
            length <- Get.getWord32be
            pairs <- replicateM (getUnsignedInt32 length) binaryToMapPair
            return $ OtpErlangMap $ Map.fromList pairs
        | tag == tagFunExt -> do
            length <- Get.lookAhead $ binaryToFunSize
            value <- Get.getByteString length
            return $ OtpErlangFunction $ E.Function
                tag value
        | tag == tagAtomUtf8Ext || tag == tagAtomExt -> do
            j <- Get.getWord16be
            value <- Get.getByteString $ getUnsignedInt16 j
            if value == boolTrue then
                return $ OtpErlangAtomBool True
            else if value == boolFalse then
                return $ OtpErlangAtomBool False
            else if tag == tagAtomUtf8Ext then
                return $ OtpErlangAtomUTF8 value
            else
                return $ OtpErlangAtom value
        | tag == tagSmallAtomUtf8Ext || tag == tagSmallAtomExt -> do
            j <- Get.getWord8
            value <- Get.getByteString $ getUnsignedInt8 j
            if value == boolTrue then
                return $ OtpErlangAtomBool True
            else if value == boolFalse then
                return $ OtpErlangAtomBool False
            else if tag == tagSmallAtomUtf8Ext then
                return $ OtpErlangAtomUTF8 value
            else
                return $ OtpErlangAtom value
        | tag == tagCompressedZlib -> do
            sizeUncompressed <- Get.getWord32be
            compressed <- Get.getRemainingLazyByteString
            let dataUncompressed = Zlib.decompress $ compressed
                size1 = fromIntegral $ getUnsignedInt32 sizeUncompressed
                size2 = LazyByteString.length dataUncompressed
            if size1 == 0 || size1 /= size2 then
                fail $ "compression corrupt"
            else
                case Get.runGetOrFail binaryToTerms dataUncompressed of
                    Left (_, _, err) ->
                        fail err
                    Right (_, _, term) ->
                        return term
        | tag == tagLocalExt ->
            fail $ "LOCAL_EXT is opaque"
        | otherwise ->
            fail $ "invalid tag"

binaryToTermSequence :: Int -> Get [OtpErlangTerm]
binaryToTermSequence length = do
    value <- replicateM length binaryToTerms
    return value

binaryToMapPair :: Get (OtpErlangTerm, OtpErlangTerm)
binaryToMapPair = do
    key <- binaryToTerms
    value <- binaryToTerms
    return (key, value)

binaryToInteger :: Get Int
binaryToInteger = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagSmallIntegerExt -> do
            value <- Get.getWord8
            return $ getUnsignedInt8 value
        | tag == tagIntegerExt -> do
            value <- Get.getWord32be
            return $ getSignedInt32 value
        | otherwise ->
            fail $ "invalid integer tag"

binaryToPid :: Get Pid
binaryToPid = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagNewPidExt -> do
            (nodeTag, node) <- binaryToAtom
            eid <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getByteString 4
            return $ E.Pid
                nodeTag node eid serial creation
        | tag == tagPidExt -> do
            (nodeTag, node) <- binaryToAtom
            eid <- Get.getByteString 4
            serial <- Get.getByteString 4
            creation <- Get.getByteString 1
            return $ E.Pid
                nodeTag node eid serial creation
        | otherwise ->
            fail $ "invalid pid tag"

binaryToExportSize :: Get Int
binaryToExportSize = do
    oldI <- Get.bytesRead
    (_, _) <- binaryToAtom -- module
    (_, _) <- binaryToAtom -- function
    arityTag <- Get.getWord8
    _ <- Get.getWord8 -- arity
    i <- Get.bytesRead
    if arityTag == tagSmallIntegerExt then
        return $ fromIntegral (i - oldI)
    else
        fail $ "invalid small integer tag"

binaryToFunSize :: Get Int
binaryToFunSize = do
    oldI <- Get.bytesRead
    numfree <- Get.getWord32be
    _ <- binaryToPid -- pid
    (_, _) <- binaryToAtom -- module
    _ <- binaryToInteger -- index
    _ <- binaryToInteger -- uniq
    _ <- binaryToTermSequence (getUnsignedInt32 numfree) -- free
    i <- Get.bytesRead
    return $ fromIntegral (i - oldI)

binaryToAtom :: Get (Word8, ByteString)
binaryToAtom = do
    tag <- Get.getWord8
    case () of
      _ | tag == tagAtomExt -> do
            j <- Get.lookAhead $ Get.getWord16be
            value <- Get.getByteString $ 2 + (getUnsignedInt16 j)
            return (tag, value)
        | tag == tagAtomCacheRef -> do
            value <- Get.getByteString 1
            return (tag, value)
        | tag == tagSmallAtomExt -> do
            j <- Get.lookAhead $ Get.getWord8
            value <- Get.getByteString $ 1 + (getUnsignedInt8 j)
            return (tag, value)
        | tag == tagAtomUtf8Ext -> do
            j <- Get.lookAhead $ Get.getWord16be
            value <- Get.getByteString $ 2 + (getUnsignedInt16 j)
            return (tag, value)
        | tag == tagSmallAtomUtf8Ext -> do
            j <- Get.lookAhead $ Get.getWord8
            value <- Get.getByteString $ 1 + (getUnsignedInt8 j)
            return (tag, value)
        | otherwise ->
            fail $ "invalid atom tag"

termsToBinary :: OtpErlangTerm -> Result LazyByteString
termsToBinary (OtpErlangInteger value)
    | value >= 0 && value <= 255 =
        ok $ Builder.toLazyByteString $
            Builder.word8 tagSmallIntegerExt <>
            Builder.word8 (fromIntegral value)
    | value >= (-2147483648) && value <= 2147483647 =
        ok $ Builder.toLazyByteString $
            Builder.word8 tagIntegerExt <>
            Builder.int32BE (fromIntegral value)
    | otherwise =
        termsToBinary $ OtpErlangIntegerBig $ fromIntegral value
termsToBinary (OtpErlangIntegerBig value) =
    let sign = if value < 0 then 1 else 0
        loop bignum l =
            if bignum > 0 then
                loop (bignum `quot` 256)
                    (LazyByteString.cons (fromIntegral $ bignum .&. 255) l)
            else
                LazyByteString.reverse l
        lResult = loop (abs value) LazyByteString.empty
        lLength = LazyByteString.length lResult in
    if lLength <= 255 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagSmallBigExt <>
            Builder.word8 (fromIntegral lLength) <>
            Builder.word8 sign <>
            Builder.lazyByteString lResult
    else if lLength <= 4294967295 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagLargeBigExt <>
            Builder.word32BE (fromIntegral lLength) <>
            Builder.word8 sign <>
            Builder.lazyByteString lResult
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangFloat value) =
    ok $ Builder.toLazyByteString $
        Builder.word8 tagNewFloatExt <>
        Builder.doubleBE value
termsToBinary (OtpErlangAtom value) =
    -- deprecated
    -- (not used in Erlang/OTP 26, i.e., minor_version 2)
    let length = ByteString.length value in
    if length <= 255 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagSmallAtomExt <>
            Builder.word8 (fromIntegral length) <>
            Builder.byteString value
    else if length <= 65535 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagAtomExt <>
            Builder.word16BE (fromIntegral length) <>
            Builder.byteString value
    else
        errorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangAtomUTF8 value) =
    let length = ByteString.length value in
    if length <= 255 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagSmallAtomUtf8Ext <>
            Builder.word8 (fromIntegral length) <>
            Builder.byteString value
    else if length <= 65535 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagAtomUtf8Ext <>
            Builder.word16BE (fromIntegral length) <>
            Builder.byteString value
    else
        errorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangAtomCacheRef value) =
    ok $ Builder.toLazyByteString $
        Builder.word8 tagAtomCacheRef <>
        Builder.word8 (fromIntegral value)
termsToBinary (OtpErlangAtomBool value) =
    if value then
        termsToBinary $ OtpErlangAtomUTF8 $ Char8.pack "true"
    else
        termsToBinary $ OtpErlangAtomUTF8 $ Char8.pack "false"
termsToBinary (OtpErlangString value) =
    let length = ByteString.length value in
    if length == 0 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagNilExt
    else if length <= 65535 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagStringExt <>
            Builder.word16BE (fromIntegral length) <>
            Builder.byteString value
    else if length <= 4294967295 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagListExt <>
            Builder.word32BE (fromIntegral length) <>
            Builder.word8 tagSmallIntegerExt <>
            Builder.byteString
                (ByteString.intersperse tagSmallIntegerExt value) <>
            Builder.word8 tagNilExt
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangBinary value) =
    let length = ByteString.length value in
    if length <= 4294967295 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagBinaryExt <>
            Builder.word32BE (fromIntegral length) <>
            Builder.byteString value
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangBinaryBits (value, 8)) =
    termsToBinary $ OtpErlangBinary value
termsToBinary (OtpErlangBinaryBits (value, bits)) =
    let length = ByteString.length value in
    if length <= 4294967295 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagBitBinaryExt <>
            Builder.word32BE (fromIntegral length) <>
            Builder.word8 (fromIntegral bits) <>
            Builder.byteString value
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangList value) =
    let length = List.length value in
    if length == 0 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagNilExt
    else if length <= 4294967295 then
        case termSequenceToBinary value Monoid.mempty of
            Left err ->
                errorType err
            Right listValue ->
                ok $ Builder.toLazyByteString $
                    Builder.word8 tagListExt <>
                    Builder.word32BE (fromIntegral length) <>
                    listValue <>
                    Builder.word8 tagNilExt
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangListImproper value) =
    let length = List.length value in
    if length == 0 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagNilExt -- misuse of OtpErlangListImproper
    else if length <= 4294967295 then
        case termSequenceToBinary value Monoid.mempty of
            Left err ->
                errorType err
            Right listValue ->
                ok $ Builder.toLazyByteString $
                    Builder.word8 tagListExt <>
                    Builder.word32BE (fromIntegral $ length - 1) <>
                    listValue
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangTuple value) =
    let length = List.length value in
    if length <= 255 then
        case termSequenceToBinary value Monoid.mempty of
            Left err ->
                errorType err
            Right tupleValue ->
                ok $ Builder.toLazyByteString $
                    Builder.word8 tagSmallTupleExt <>
                    Builder.word8 (fromIntegral length) <>
                    tupleValue
    else if length <= 4294967295 then
        case termSequenceToBinary value Monoid.mempty of
            Left err ->
                errorType err
            Right tupleValue ->
                ok $ Builder.toLazyByteString $
                    Builder.word8 tagLargeTupleExt <>
                    Builder.word32BE (fromIntegral length) <>
                    tupleValue
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangMap value) =
    let length = Map.size value in
    if length <= 4294967295 then
        case Map.foldlWithKey mapPairToBinary (Right Monoid.mempty) value of
            Left err ->
                errorType err
            Right mapValue ->
                ok $ Builder.toLazyByteString $
                    Builder.word8 tagMapExt <>
                    Builder.word32BE (fromIntegral length) <>
                    mapValue
    else
        errorType $ OutputError "uint32 overflow"
termsToBinary (OtpErlangPid (E.Pid nodeTag node eid serial creation)) =
    let tag =
            if (ByteString.length creation) == 4 then
                tagNewPidExt
            else
                tagPidExt in
    ok $ Builder.toLazyByteString $
        Builder.word8 tag <>
        Builder.word8 nodeTag <>
        Builder.byteString node <>
        Builder.byteString eid <>
        Builder.byteString serial <>
        Builder.byteString creation
termsToBinary (OtpErlangPort (E.Port nodeTag node eid creation)) =
    let tag =
            if (ByteString.length eid) == 8 then
                tagV4PortExt
            else if (ByteString.length creation) == 4 then
                tagNewPortExt
            else
                tagPortExt in
    ok $ Builder.toLazyByteString $
        Builder.word8 tag <>
        Builder.word8 nodeTag <>
        Builder.byteString node <>
        Builder.byteString eid <>
        Builder.byteString creation
termsToBinary (OtpErlangReference (E.Reference nodeTag node eid creation)) =
    let length = (ByteString.length eid) `quot` 4 in
    if length == 0 then
        ok $ Builder.toLazyByteString $
            Builder.word8 tagReferenceExt <>
            Builder.word8 nodeTag <>
            Builder.byteString node <>
            Builder.byteString eid <>
            Builder.byteString creation
    else if length <= 65535 then
        let tag =
                if (ByteString.length creation) == 4 then
                    tagNewerReferenceExt
                else
                    tagNewReferenceExt in
        ok $ Builder.toLazyByteString $
            Builder.word8 tag <>
            Builder.word16BE (fromIntegral length) <>
            Builder.word8 nodeTag <>
            Builder.byteString node <>
            Builder.byteString creation <>
            Builder.byteString eid
    else
        errorType $ OutputError "uint16 overflow"
termsToBinary (OtpErlangFunction (E.Function tag value)) =
    ok $ Builder.toLazyByteString $
        Builder.word8 tag <>
        Builder.byteString value

termSequenceToBinary :: [OtpErlangTerm] -> Builder -> Result Builder
termSequenceToBinary [] builder =
    ok builder
termSequenceToBinary (h:t) builder =
    case termsToBinary h of
        Left err ->
            errorType err
        Right binary ->
            termSequenceToBinary t (builder <> Builder.lazyByteString binary)

mapPairToBinary :: Result Builder -> OtpErlangTerm -> OtpErlangTerm ->
    Result Builder
mapPairToBinary (Left err) _ _ =
    errorType err
mapPairToBinary (Right builder) key value =
    case termsToBinary key of
        Left err ->
            errorType err
        Right binaryKey ->
            case termsToBinary value of
                Left err ->
                    errorType err
                Right binaryValue ->
                    ok $ builder <>
                        Builder.lazyByteString binaryKey <>
                        Builder.lazyByteString binaryValue

