{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  MIT License

  Copyright (c) 2017-2020 Michael Truog <mjtruog at protonmail dot com>

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

module Foreign.CloudI.Instance
    ( RequestType(..)
    , Source
    , Response(..)
    , Callback
    , T(..)
    , make
    , init
    , reinit
    , setResponse
    , setTransId
    , setTransIds
    , setSubscribeCount
    , callbacksAdd
    , callbacksRemove
    ) where

import Prelude hiding (init)
import Data.Typeable (Typeable)
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as Int
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Sequence as Sequence
import qualified Data.Word as Word
import qualified Foreign.C.Types as C
import qualified Foreign.Erlang.Pid as Erlang
import qualified Network.Socket as Socket
import qualified System.IO as SysIO
type Array = IArray.Array
type Builder = Builder.Builder
type ByteString = ByteString.ByteString
type Handle = SysIO.Handle
type Int8 = Int.Int8
type Map = Map.Map
type Seq = Sequence.Seq
type Socket = Socket.Socket
type Word32 = Word.Word32

-- | provided when handling a service request
data RequestType =
      ASYNC
    | SYNC
    deriving (Eq, Show)

-- | the Erlang pid that is the source of the service request
type Source = Erlang.Pid

-- | a function to handle a service request
type Callback s =
    RequestType ->
    ByteString -> ByteString ->
    ByteString -> ByteString ->
    Int -> Int -> ByteString -> Source ->
    s -> T s ->
    IO (Response s)

-- | service request callback function return type
data Response s =
      Response (ByteString, s, T s)
    | ResponseInfo (ByteString, ByteString, s, T s)
    | Forward (ByteString, ByteString, ByteString, s, T s)
    | Forward_ (ByteString, ByteString, ByteString, Int, Int, s, T s)
    | Null (s, T s)
    | NullError (String, s, T s)
    deriving (Show, Typeable)

-- | an instance of the CloudI API
data T s = T
    { state :: !s
    , terminateException :: !Bool
    , socketHandle :: !Handle
    , useHeader :: !Bool
    , initializationComplete :: !Bool
    , terminate :: !Bool
    , timeout :: !(Maybe Bool)
    , callbacks :: !(Map ByteString (Seq (Callback s)))
    , bufferSize :: !Int
    , bufferRecv :: !Builder
    , bufferRecvSize :: !Int
    , processIndex :: !Int
    , processCount :: !Int
    , processCountMax :: !Int
    , processCountMin :: !Int
    , prefix :: !ByteString
    , timeoutInitialize :: !Int
    , timeoutAsync :: !Int
    , timeoutSync :: !Int
    , timeoutTerminate :: !Int
    , priorityDefault :: !Int
    , responseInfo :: !ByteString
    , response :: !ByteString
    , transId :: !ByteString
    , transIds :: !(Array Int ByteString)
    , subscribeCount :: !Int
    }
    deriving (Typeable)

instance Show (T s) where
    show _ = ""

makeSocket :: String -> C.CInt -> IO Socket
makeSocket "local" fd =
    Socket.mkSocket fd
makeSocket "tcp" fd =
    Socket.mkSocket fd
makeSocket "udp" fd =
    Socket.mkSocket fd
makeSocket _ _ =
    error "invalid protocol"

makeSocketHandle :: String -> C.CInt -> IO Handle
makeSocketHandle protocol fd = do
    socket <- makeSocket protocol fd
    Socket.socketToHandle socket SysIO.ReadWriteMode

make :: s -> Bool -> String -> C.CInt -> Bool -> Int -> Int -> IO (T s)
make state' terminateException'
    protocol fd useHeader' bufferSize' timeoutTerminate' = do
    socketHandle' <- makeSocketHandle protocol fd
    return $ T {
          state = state'
        , terminateException = terminateException'
        , socketHandle = socketHandle'
        , useHeader = useHeader'
        , initializationComplete = False
        , terminate = False
        , timeout = Nothing
        , callbacks = Map.empty
        , bufferSize = bufferSize'
        , bufferRecv = Monoid.mempty
        , bufferRecvSize = 0
        , processIndex = 0
        , processCount = 0
        , processCountMax = 0
        , processCountMin = 0
        , prefix = ByteString.empty
        , timeoutInitialize = 0
        , timeoutAsync = 0
        , timeoutSync = 0
        , timeoutTerminate = timeoutTerminate'
        , priorityDefault = 0
        , responseInfo = ByteString.empty
        , response = ByteString.empty
        , transId = ByteString.empty
        , transIds = IArray.array (0, 0) [(0, ByteString.empty)]
        , subscribeCount = 0
    }

init :: T s -> Word32 -> Word32 -> Word32 -> Word32 -> ByteString ->
    Word32 -> Word32 -> Word32 -> Word32 -> Int8 -> T s
init api0
    processIndex' processCount' processCountMax' processCountMin'
    prefix' timeoutInitialize' timeoutAsync' timeoutSync' timeoutTerminate'
    priorityDefault' =
    api0{
          timeout = Just False
        , processIndex = fromIntegral processIndex'
        , processCount = fromIntegral processCount'
        , processCountMax = fromIntegral processCountMax'
        , processCountMin = fromIntegral processCountMin'
        , prefix = prefix'
        , timeoutInitialize = fromIntegral timeoutInitialize'
        , timeoutAsync = fromIntegral timeoutAsync'
        , timeoutSync = fromIntegral timeoutSync'
        , timeoutTerminate = fromIntegral timeoutTerminate'
        , priorityDefault = fromIntegral priorityDefault'}

reinit :: T s -> Word32 -> Word32 -> Word32 -> Int8 -> T s
reinit api0
    processCount' timeoutAsync' timeoutSync'
    priorityDefault' =
    api0{
          processCount = fromIntegral processCount'
        , timeoutAsync = fromIntegral timeoutAsync'
        , timeoutSync = fromIntegral timeoutSync'
        , priorityDefault = fromIntegral priorityDefault'}

setResponse :: T s -> ByteString -> ByteString -> ByteString -> T s
setResponse api0
    responseInfo' response' transId' =
    api0{
          timeout = Just False
        , responseInfo = responseInfo'
        , response = response'
        , transId = transId'}

setTransId :: T s -> ByteString -> T s
setTransId api0
    transId' =
    api0{
          timeout = Just False
        , transId = transId'}

setTransIds :: T s -> ByteString -> Word32 -> T s
setTransIds api0
    transIds' transIdCount =
    let count = fromIntegral transIdCount :: Int
        loop i l s =
            if i == count then
                l
            else
                let (e, s') = ByteString.splitAt 16 s in
                loop (i + 1) ((i, e):l) s'
    in
    api0{
          timeout = Just False
        , transIds = IArray.array (0, count - 1) (loop 0 [] transIds')}

setSubscribeCount :: T s -> Word32 -> T s
setSubscribeCount api0
    subscribeCount' =
    api0{
          timeout = Just False
        , subscribeCount = fromIntegral subscribeCount'}

callbacksAdd :: T s -> ByteString -> Callback s -> T s
callbacksAdd api0@T{
      callbacks = callbacks0
    , prefix = prefix'} pattern f =
    let key = ByteString.append prefix' pattern
        callbacks1 = case Map.lookup key callbacks0 of
            Nothing ->
                Map.insert key (Sequence.singleton f) callbacks0
            Just functionQueue ->
                Map.insert key ((Sequence.|>) functionQueue f) callbacks0
    in
    api0{callbacks = callbacks1}

callbacksRemove :: T s -> ByteString -> T s
callbacksRemove api0@T{
      callbacks = callbacks0
    , prefix = prefix'} pattern =
    let key = ByteString.append prefix' pattern
        callbacks1 = case Map.lookup key callbacks0 of
            Nothing ->
                error "callbacks empty"
            Just functionQueue ->
                let functionQueueNew = Sequence.drop 0 functionQueue in
                if Sequence.null functionQueueNew then
                    Map.delete key callbacks0
                else
                    Map.insert key functionQueueNew callbacks0
    in
    api0{callbacks = callbacks1}

