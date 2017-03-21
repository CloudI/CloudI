{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  BSD LICENSE

  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
      * All advertising materials mentioning features or use of this
        software must display the following acknowledgment:
          This product includes software developed by Michael Truog
      * The name of the author may not be used to endorse or promote
        products derived from this software without specific prior
        written permission

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.

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
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Int as Int
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Sequence as Sequence
import qualified Data.Time.Clock as Clock
import qualified Data.Typeable as Typeable
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
type Word8 = Word.Word8
type Word32 = Word.Word32

data RequestType =
      ASYNC
    | SYNC
    deriving (Eq, Show)

type Source = Erlang.Pid

type Callback s =
    RequestType ->
    ByteString -> ByteString ->
    ByteString -> ByteString ->
    Int -> Int -> ByteString -> Source ->
    s -> T s ->
    IO (Response s)

data Response s =
      Response (ByteString, s, T s)
    | ResponseInfo (ByteString, ByteString, s, T s)
    | Null (s, T s)
    | NullError (String, s, T s)
    deriving (Show, Typeable.Typeable)

data T s = T
    { state :: !s
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
    , requestTimeoutAdjustment :: !Bool
    , requestTimer :: !Clock.NominalDiffTime
    , requestTimeout :: !Int
    , responseInfo :: !ByteString
    , response :: !ByteString
    , transId :: !ByteString
    , transIds :: !(Array Int ByteString)
    , subscribeCount :: !Int
    }
    deriving (Typeable.Typeable)

instance Show (T s) where
    show _ = ""

makeSocket :: String -> C.CInt -> IO Socket
makeSocket "local" fd =
    Socket.mkSocket fd Socket.AF_UNIX Socket.Stream
        Socket.defaultProtocol Socket.Connected
makeSocket "tcp" fd =
    Socket.mkSocket fd Socket.AF_INET Socket.Stream
        Socket.defaultProtocol Socket.Connected
makeSocket "udp" fd =
    Socket.mkSocket fd Socket.AF_INET Socket.Datagram
        Socket.defaultProtocol Socket.Connected
makeSocket _ _ =
    error "invalid protocol"

makeSocketHandle :: String -> C.CInt -> IO Handle
makeSocketHandle protocol fd = do
    socket <- makeSocket protocol fd
    Socket.socketToHandle socket SysIO.ReadWriteMode

make :: s -> String -> C.CInt -> Bool -> Int -> Int -> IO (T s)
make state' protocol fd useHeader' bufferSize' timeoutTerminate' = do
    socketHandle' <- makeSocketHandle protocol fd
    return $ T {
          state = state'
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
        , requestTimeoutAdjustment = False
        , requestTimer = 0
        , requestTimeout = 0
        , responseInfo = ByteString.empty
        , response = ByteString.empty
        , transId = ByteString.empty
        , transIds = IArray.array (0, 0) [(0, ByteString.empty)]
        , subscribeCount = 0
    }

init :: T s -> Word32 -> Word32 -> Word32 -> Word32 -> ByteString ->
    Word32 -> Word32 -> Word32 -> Word32 -> Int8 -> Word8 -> T s
init api0
    processIndex' processCount' processCountMax' processCountMin'
    prefix' timeoutInitialize' timeoutAsync' timeoutSync' timeoutTerminate'
    priorityDefault' requestTimeoutAdjustment' =
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
        , priorityDefault = fromIntegral priorityDefault'
        , requestTimeoutAdjustment = requestTimeoutAdjustment' /= 0}

reinit :: T s -> Word32 -> Word32 -> Word32 -> Int8 -> Word8 -> T s
reinit api0
    processCount' timeoutAsync' timeoutSync'
    priorityDefault' requestTimeoutAdjustment' =
    api0{
          processCount = fromIntegral processCount'
        , timeoutAsync = fromIntegral timeoutAsync'
        , timeoutSync = fromIntegral timeoutSync'
        , priorityDefault = fromIntegral priorityDefault'
        , requestTimeoutAdjustment = requestTimeoutAdjustment' /= 0}

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

