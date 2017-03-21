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

module Foreign.CloudI
    ( Instance.RequestType(..)
    , Instance.Source
    , Instance.Response(..)
    , Instance.Callback
    , Instance.T
    , invalidInputError
    , messageDecodingError
    , terminateError
    , Exception
    , api
    , threadCount
    , poll
    , threadCreate
    , threadsWait
    ) where

import Prelude hiding (init,length)
import Data.Bits (shiftL,(.|.))
import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent
import qualified Data.Monoid as Monoid
import qualified Data.Binary.Get as Get
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX (getPOSIXTime)
import qualified Data.Typeable as Typeable
import qualified System.Posix.Env as POSIX (getEnv)
import qualified Data.Word as Word
import qualified System.IO as SysIO
import qualified System.IO.Unsafe as Unsafe
import qualified Foreign.C.Types as C
import qualified Foreign.Erlang as Erlang
import qualified Foreign.CloudI.Instance as Instance
type Get = Get.Get
type Builder = Builder.Builder
type ByteString = ByteString.ByteString
type LazyByteString = LazyByteString.ByteString
type Word32 = Word.Word32
type Handle = SysIO.Handle
type RequestType = Instance.RequestType
type Source = Instance.Source
type ThreadId = Concurrent.ThreadId
type SomeException = Exception.SomeException

messageInit :: Word32
messageInit = 1
messageSendAsync :: Word32
messageSendAsync = 2
messageSendSync :: Word32
messageSendSync = 3
messageRecvAsync :: Word32
messageRecvAsync = 4
messageReturnAsync :: Word32
messageReturnAsync = 5
messageReturnSync :: Word32
messageReturnSync = 6
messageReturnsAsync :: Word32
messageReturnsAsync = 7
messageKeepalive :: Word32
messageKeepalive = 8
messageReinit :: Word32
messageReinit = 9
messageSubscribeCount :: Word32
messageSubscribeCount = 10
messageTerm :: Word32
messageTerm = 11

data Message =
      MessageSend (
        RequestType, ByteString, ByteString, ByteString, ByteString,
        Int, Int, ByteString, Source)
    | MessageKeepalive

invalidInputError :: String
invalidInputError = "Invalid Input"
messageDecodingError :: String
messageDecodingError = "Message Decoding Error"
terminateError :: String
terminateError = "Terminate"

data Exception s =
      ReturnSync (Instance.T s)
    | ReturnAsync (Instance.T s)
    | ForwardSync (Instance.T s)
    | ForwardAsync (Instance.T s)
    deriving (Show, Typeable.Typeable)

--instance Typeable.Typeable (Exception s) => Exception.Exception (Exception s)
instance Typeable.Typeable s => Exception.Exception (Exception s)

printException :: String -> IO ()
printException str =
    SysIO.hPutStrLn SysIO.stderr ("Exception: " ++ str)

printError :: String -> IO ()
printError str =
    SysIO.hPutStrLn SysIO.stderr ("Error: " ++ str)

data CallbackResult s =
      Return (ByteString, ByteString, s, Instance.T s)
    | Finished (Instance.T s)

type Result a = Either String a

infixr 4 <>
(<>) :: Monoid.Monoid m => m -> m -> m
(<>) = Monoid.mappend

timeoutAdjustment :: Clock.NominalDiffTime -> Int ->
    IO (Clock.NominalDiffTime, Int)
timeoutAdjustment t0 timeout = do
    t1 <- POSIX.getPOSIXTime
    if t1 <= t0 then
        return (t1, timeout)
    else
        let elapsed = floor ((t1 - t0) * 1000) :: Integer
            timeoutValue = fromIntegral timeout :: Integer in
        if elapsed >= timeoutValue then
            return (t1, 0)
        else
            return (t1, fromIntegral (timeoutValue - elapsed))

send :: Instance.T s -> LazyByteString -> IO ()
send Instance.T{
      Instance.useHeader = False
    , Instance.socketHandle = socketHandle} binary = do
    LazyByteString.hPut socketHandle binary
send Instance.T{
      Instance.useHeader = True
    , Instance.socketHandle = socketHandle} binary = do
    let total = fromIntegral (LazyByteString.length binary) :: Word32
    LazyByteString.hPut socketHandle (Builder.toLazyByteString $
        Builder.word32BE total `Monoid.mappend`
        Builder.lazyByteString binary)

recvBuffer :: Builder -> Int -> Int -> Handle -> Int ->
    IO (Builder, Int)
recvBuffer bufferRecv bufferRecvSize recvSize socketHandle bufferSize
    | recvSize <= bufferRecvSize =
        return (bufferRecv, bufferRecvSize)
    | otherwise = do
        fragment <- ByteString.hGetNonBlocking socketHandle bufferSize
        recvBuffer
            (bufferRecv <> Builder.byteString fragment)
            (bufferRecvSize + ByteString.length fragment)
            recvSize socketHandle bufferSize

recvBufferAll :: Builder -> Int -> Handle -> Int ->
    IO (Builder, Int)
recvBufferAll bufferRecv bufferRecvSize socketHandle bufferSize = do
    fragment <- ByteString.hGetNonBlocking socketHandle bufferSize
    let fragmentSize = ByteString.length fragment
        bufferRecvNew = bufferRecv <> Builder.byteString fragment
        bufferRecvSizeNew = bufferRecvSize + fragmentSize
    if fragmentSize == bufferSize then
        recvBufferAll bufferRecvNew bufferRecvSizeNew socketHandle bufferSize
    else
        return (bufferRecv, bufferRecvSize)

recv :: Instance.T s -> IO (LazyByteString, Int, Instance.T s)
recv api0@Instance.T{
      Instance.useHeader = False
    , Instance.socketHandle = socketHandle
    , Instance.bufferSize = bufferSize
    , Instance.bufferRecv = bufferRecv
    , Instance.bufferRecvSize = bufferRecvSize} = do
    (bufferRecvAll,
     bufferRecvAllSize) <- recvBufferAll
        bufferRecv bufferRecvSize socketHandle bufferSize
    return $ (
          Builder.toLazyByteString bufferRecvAll
        , bufferRecvAllSize
        , api0{
              Instance.bufferRecv = Monoid.mempty
            , Instance.bufferRecvSize = 0})
recv api0@Instance.T{
      Instance.useHeader = True
    , Instance.socketHandle = socketHandle
    , Instance.bufferSize = bufferSize
    , Instance.bufferRecv = bufferRecv
    , Instance.bufferRecvSize = bufferRecvSize} = do
    (bufferRecvHeader, bufferRecvHeaderSize) <- recvBuffer
        bufferRecv bufferRecvSize 4 socketHandle bufferSize
    let header0 = Builder.toLazyByteString bufferRecvHeader
        Just (byte0, header1) = LazyByteString.uncons header0
        Just (byte1, header2) = LazyByteString.uncons header1
        Just (byte2, header3) = LazyByteString.uncons header2
        Just (byte3, headerRemaining) = LazyByteString.uncons header3
        total = fromIntegral $
            (fromIntegral byte0 :: Word32) `shiftL` 24 .|.
            (fromIntegral byte1 :: Word32) `shiftL` 16 .|.
            (fromIntegral byte2 :: Word32) `shiftL` 8 .|.
            (fromIntegral byte3 :: Word32) :: Int
    (bufferRecvAll, bufferRecvAllSize) <- recvBuffer
        (Builder.lazyByteString headerRemaining)
        (bufferRecvHeaderSize - 4) total socketHandle bufferSize
    let bufferRecvAllStr = Builder.toLazyByteString bufferRecvAll
        (bufferRecvData, bufferRecvNew) =
            LazyByteString.splitAt (fromIntegral total) bufferRecvAllStr
    return $ (
          bufferRecvData
        , total
        , api0{
              Instance.bufferRecv = Builder.lazyByteString bufferRecvNew
            , Instance.bufferRecvSize = bufferRecvAllSize - total})

api :: Typeable.Typeable s => Int -> s -> IO (Result (Instance.T s))
api threadIndex state = do
    SysIO.hSetEncoding SysIO.stdout SysIO.utf8
    SysIO.hSetBuffering SysIO.stdout SysIO.LineBuffering
    SysIO.hSetEncoding SysIO.stderr SysIO.utf8
    SysIO.hSetBuffering SysIO.stderr SysIO.LineBuffering
    protocolValue <- POSIX.getEnv "CLOUDI_API_INIT_PROTOCOL"
    bufferSizeValue <- POSIX.getEnv "CLOUDI_API_INIT_BUFFER_SIZE"
    case (protocolValue, bufferSizeValue) of
        (Nothing, _) ->
            return $ Left invalidInputError
        (_, Nothing) ->
            return $ Left invalidInputError
        (Just protocol, Just bufferSizeStr) ->
            let bufferSize = read bufferSizeStr :: Int
                fd = C.CInt $ fromIntegral (threadIndex + 3)
                useHeader = protocol /= "udp"
                timeoutTerminate = 1000
                initTerms = Erlang.OtpErlangAtom (Char8.pack "init")
            in
            case Erlang.termToBinary initTerms (-1) of
                Left err ->
                    return $ Left $ show err
                Right initBinary -> do
                    api0 <- Instance.make state protocol fd
                        useHeader bufferSize timeoutTerminate
                    send api0 initBinary
                    result <- pollRequest api0 (-1) False
                    case result of
                        Left err -> do
                            return $ Left err
                        Right (_, api1) -> do
                            return $ Right api1

threadCount :: IO (Result Int)
threadCount = do
    threadCountValue <- POSIX.getEnv "CLOUDI_API_INIT_THREAD_COUNT"
    case threadCountValue of
        Nothing ->
            return $ Left invalidInputError
        Just threadCountStr ->
            return $ Right (read threadCountStr :: Int)

returnAsyncI :: Instance.T s -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> ByteString -> Source ->
    IO (Result (Instance.T s))
returnAsyncI api0@Instance.T{
      Instance.requestTimeoutAdjustment = requestTimeoutAdjustment
    , Instance.requestTimer = requestTimer
    , Instance.requestTimeout = requestTimeout}
    name pattern responseInfo response timeout transId pid = do
    (_, timeoutNew) <-
        if requestTimeoutAdjustment && timeout == requestTimeout then
            timeoutAdjustment requestTimer timeout
        else
            return (0, timeout)
    let returnTerms = Erlang.OtpErlangTuple
            [ Erlang.OtpErlangAtom (Char8.pack "return_async")
            , Erlang.OtpErlangString name
            , Erlang.OtpErlangString pattern
            , Erlang.OtpErlangBinary responseInfo
            , Erlang.OtpErlangBinary response
            , Erlang.OtpErlangInteger timeoutNew
            , Erlang.OtpErlangBinary transId
            , Erlang.OtpErlangPid pid]
    case Erlang.termToBinary returnTerms (-1) of
        Left err ->
            return $ Left $ show err
        Right returnBinary -> do
            send api0 returnBinary
            return $ Right api0

returnSyncI :: Instance.T s -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> ByteString -> Source ->
    IO (Result (Instance.T s))
returnSyncI api0@Instance.T{
      Instance.requestTimeoutAdjustment = requestTimeoutAdjustment
    , Instance.requestTimer = requestTimer
    , Instance.requestTimeout = requestTimeout}
    name pattern responseInfo response timeout transId pid = do
    (_, timeoutNew) <-
        if requestTimeoutAdjustment && timeout == requestTimeout then
            timeoutAdjustment requestTimer timeout
        else
            return (0, timeout)
    let returnTerms = Erlang.OtpErlangTuple
            [ Erlang.OtpErlangAtom (Char8.pack "return_sync")
            , Erlang.OtpErlangString name
            , Erlang.OtpErlangString pattern
            , Erlang.OtpErlangBinary responseInfo
            , Erlang.OtpErlangBinary response
            , Erlang.OtpErlangInteger timeoutNew
            , Erlang.OtpErlangBinary transId
            , Erlang.OtpErlangPid pid]
    case Erlang.termToBinary returnTerms (-1) of
        Left err ->
            return $ Left $ show err
        Right returnBinary -> do
            send api0 returnBinary
            return $ Right api0

nullResponse :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    s -> Instance.T s -> IO (Instance.Response s)
nullResponse _ _ _ _ _ _ _ _ _ state api0 =
    return $ Instance.Null (state, api0)

callback :: Typeable.Typeable s => Instance.T s ->
    (RequestType, ByteString, ByteString, ByteString, ByteString,
     Int, Int, ByteString, Source) -> IO (Result (Instance.T s))
callback api0@Instance.T{
      Instance.state = state
    , Instance.callbacks = callbacks
    , Instance.requestTimeoutAdjustment = requestTimeoutAdjustment}
    (requestType, name, pattern, requestInfo, request,
     timeout, priority, transId, pid) = do
    api1 <- if requestTimeoutAdjustment then do
            requestTimer <- POSIX.getPOSIXTime
            return api0{
                  Instance.requestTimer = requestTimer
                , Instance.requestTimeout = timeout}
        else
            return api0
    let (callbackF, callbacksNew) = case Map.lookup pattern callbacks of
            Nothing ->
                (nullResponse, callbacks)
            Just functionQueue ->
                let f = Sequence.index functionQueue 0
                    functionQueueNew = (Sequence.|>)
                        (Sequence.drop 0 functionQueue) f
                in
                (f, Map.insert pattern functionQueueNew callbacks)
        api2 = api1{Instance.callbacks = callbacksNew}
        empty = ByteString.empty
    callbackResultValue <- Exception.try $ case requestType of
        Instance.ASYNC -> do
            callbackResultAsyncValue <- Exception.try $
                callbackF requestType name pattern
                requestInfo request timeout priority transId pid
                state api2
            case callbackResultAsyncValue of
                Left (ReturnSync api3) -> do
                    printException "Synchronous Call Return Invalid"
                    return $ Finished api3
                Left (ReturnAsync api3) ->
                    return $ Finished api3
                Left (ForwardSync api3) -> do
                    printException "Synchronous Call Forward Invalid"
                    return $ Finished api3
                Left (ForwardAsync api3) ->
                    return $ Finished api3
                Right (Instance.ResponseInfo values) ->
                    return $ Return values
                Right (Instance.Response (value1, value2, value3)) ->
                    return $ Return (empty, value1, value2, value3)
                Right (Instance.Null (value1, value2)) ->
                    return $ Return (empty, empty, value1, value2)
                Right (Instance.NullError (err, value1, value2)) -> do
                    printError err
                    return $ Return (empty, empty, value1, value2)
        Instance.SYNC -> do
            callbackResultSyncValue <- Exception.try $
                callbackF requestType name pattern
                requestInfo request timeout priority transId pid
                state api2
            case callbackResultSyncValue of
                Left (ReturnSync api3) ->
                    return $ Finished api3
                Left (ReturnAsync api3) -> do
                    printException "Asynchronous Call Return Invalid"
                    return $ Finished api3
                Left (ForwardSync api3) ->
                    return $ Finished api3
                Left (ForwardAsync api3) -> do
                    printException "Asynchronous Call Forward Invalid"
                    return $ Finished api3
                Right (Instance.ResponseInfo values) ->
                    return $ Return values
                Right (Instance.Response (value1, value2, value3)) ->
                    return $ Return (empty, value1, value2, value3)
                Right (Instance.Null (value1, value2)) ->
                    return $ Return (empty, empty, value1, value2)
                Right (Instance.NullError (err, value1, value2)) -> do
                    printError err
                    return $ Return (empty, empty, value1, value2)
    callbackResultType <- case callbackResultValue of
        Left exception -> do
            printException $ show (exception :: Exception.SomeException)
            return $ Finished api2
        Right callbackResult ->
            return $ callbackResult
    case requestType of
        Instance.ASYNC ->
            case callbackResultType of
                Finished api4 ->
                    return $ Right api4
                Return (responseInfo, response, stateNew, api4) ->
                    returnAsyncI api4{Instance.state = stateNew}
                        name pattern responseInfo response
                        timeout transId pid
        Instance.SYNC ->
            case callbackResultType of
                Finished api4 ->
                    return $ Right api4
                Return (responseInfo, response, stateNew, api4) ->
                    returnSyncI api4{Instance.state = stateNew}
                        name pattern responseInfo response
                        timeout transId pid

handleEvents :: [Message] -> Instance.T s -> Bool -> Word32 ->
    Get ([Message], Instance.T s)
handleEvents messages api0 external cmd0 = do
    cmd <- if cmd0 == 0 then Get.getWord32host else return cmd0
    case () of
      _ | cmd == messageTerm ->
            let api1 = api0{
                      Instance.terminate = True
                    , Instance.timeout = Just False} in
            if external then
                return ([], api1)
            else
                fail terminateError
        | cmd == messageReinit -> do
            processCount <- Get.getWord32host
            timeoutAsync <- Get.getWord32host
            timeoutSync <- Get.getWord32host
            priorityDefault <- Get.getInt8
            requestTimeoutAdjustment <- Get.getWord8
            let api1 = Instance.reinit api0
                    processCount
                    timeoutAsync
                    timeoutSync
                    priorityDefault
                    requestTimeoutAdjustment
            empty <- Get.isEmpty
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageKeepalive -> do
            let messagesNew = MessageKeepalive:messages
            empty <- Get.isEmpty
            if not empty then
                handleEvents messagesNew api0 external 0
            else
                return (messagesNew, api0)
        | otherwise ->
            fail messageDecodingError

pollRequestDataGet :: [Message] -> Instance.T s -> Bool ->
    Get ([Message], Instance.T s)
pollRequestDataGet messages api0 external = do
    cmd <- Get.getWord32host
    case () of
      _ | cmd == messageInit -> do
            processIndex <- Get.getWord32host
            processCount <- Get.getWord32host
            processCountMax <- Get.getWord32host
            processCountMin <- Get.getWord32host
            prefixSize <- Get.getWord32host
            prefix <- Get.getByteString $ fromIntegral prefixSize - 1
            Get.skip 1
            timeoutInitialize <- Get.getWord32host
            timeoutAsync <- Get.getWord32host
            timeoutSync <- Get.getWord32host
            timeoutTerminate <- Get.getWord32host
            priorityDefault <- Get.getInt8
            requestTimeoutAdjustment <- Get.getWord8
            let api1 = Instance.init api0
                    processIndex
                    processCount
                    processCountMax
                    processCountMin
                    prefix
                    timeoutInitialize
                    timeoutAsync
                    timeoutSync
                    timeoutTerminate
                    priorityDefault
                    requestTimeoutAdjustment
            empty <- Get.isEmpty
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageSendAsync || cmd == messageSendSync -> do
            nameSize <- Get.getWord32host
            name <- Get.getByteString $ fromIntegral nameSize - 1
            Get.skip 1
            patternSize <- Get.getWord32host
            pattern <- Get.getByteString $ fromIntegral patternSize - 1
            Get.skip 1
            requestInfoSize <- Get.getWord32host
            requestInfo <- Get.getByteString $ fromIntegral requestInfoSize
            requestSize <- Get.getWord32host
            request <- Get.getByteString $ fromIntegral requestSize
            timeout <- Get.getWord32host
            priority <- Get.getInt8
            transId <- Get.getByteString 16
            pidSize <- Get.getWord32host
            pidData <- Get.getLazyByteString $ fromIntegral pidSize
            empty <- Get.isEmpty
            case Erlang.binaryToTerm pidData of
                Left err ->
                    fail $ show err
                Right (Erlang.OtpErlangPid (pid)) ->
                    let requestType =
                            if cmd == messageSendAsync then
                                Instance.ASYNC
                            else -- cmd == messageSendSync
                                Instance.SYNC
                        messagesNew = (MessageSend (
                            requestType, name, pattern, requestInfo, request,
                            fromIntegral timeout, fromIntegral priority,
                            transId, pid)):messages in
                    if not empty then
                        handleEvents messagesNew api0 external 0
                    else
                        return (messagesNew, api0)
                Right _ ->
                    fail messageDecodingError
        | cmd == messageRecvAsync || cmd == messageReturnSync -> do
            responseInfoSize <- Get.getWord32host
            responseInfo <- Get.getByteString $ fromIntegral responseInfoSize
            responseSize <- Get.getWord32host
            response <- Get.getByteString $ fromIntegral responseSize
            transId <- Get.getByteString 16
            empty <- Get.isEmpty
            let api1 = Instance.setResponse api0
                    responseInfo response transId
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageReturnAsync -> do
            transId <- Get.getByteString 16
            empty <- Get.isEmpty
            let api1 = Instance.setTransId api0
                    transId
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageReturnsAsync -> do
            transIdCount <- Get.getWord32host
            transIds <- Get.getByteString $ 16 * (fromIntegral transIdCount)
            empty <- Get.isEmpty
            let api1 = Instance.setTransIds api0
                    transIds transIdCount
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageSubscribeCount -> do
            subscribeCount <- Get.getWord32host
            empty <- Get.isEmpty
            let api1 = Instance.setSubscribeCount api0
                    subscribeCount
            if not empty then
                handleEvents messages api1 external 0
            else
                return (messages, api1)
        | cmd == messageTerm ->
            handleEvents messages api0 external cmd
        | cmd == messageReinit -> do
            processCount <- Get.getWord32host
            timeoutAsync <- Get.getWord32host
            timeoutSync <- Get.getWord32host
            priorityDefault <- Get.getInt8
            requestTimeoutAdjustment <- Get.getWord8
            let api1 = Instance.reinit api0
                    processCount
                    timeoutAsync
                    timeoutSync
                    priorityDefault
                    requestTimeoutAdjustment
            empty <- Get.isEmpty
            if not empty then
                pollRequestDataGet messages api1 external
            else
                return (messages, api1)
        | cmd == messageKeepalive -> do
            let messagesNew = MessageKeepalive:messages
            empty <- Get.isEmpty
            if not empty then
                pollRequestDataGet messagesNew api0 external
            else
                return (messagesNew, api0)
        | otherwise ->
            fail messageDecodingError

pollRequestDataProcess :: Typeable.Typeable s => [Message] -> Instance.T s ->
    IO (Result (Instance.T s))
pollRequestDataProcess [] api0 =
    return $ Right api0
pollRequestDataProcess (message:messages) api0 =
    case message of
        MessageSend callbackData -> do
            callbackResult <- callback api0 callbackData
            case callbackResult of
                Left err ->
                    return $ Left err
                Right api1 ->
                    pollRequestDataProcess messages api1
        MessageKeepalive ->
            let aliveTerms = Erlang.OtpErlangAtom (Char8.pack "keepalive") in
            case Erlang.termToBinary aliveTerms (-1) of
                Left err ->
                    return $ Left $ show err
                Right aliveBinary -> do
                    send api0 aliveBinary
                    pollRequestDataProcess messages api0

pollRequestData :: Typeable.Typeable s =>
    Instance.T s -> Bool -> LazyByteString ->
    IO (Result (Instance.T s))
pollRequestData api0 external dataIn =
    case Get.runGetOrFail (pollRequestDataGet [] api0 external) dataIn of
        Left (_, _, err) ->
            return $ Left err
        Right (_, _, (messages, api1)) ->
            pollRequestDataProcess (List.reverse messages) api1

pollRequestLoop :: Typeable.Typeable s =>
    Instance.T s -> Int -> Bool -> Clock.NominalDiffTime ->
    IO (Result (Bool, Instance.T s))
pollRequestLoop api0@Instance.T{
      Instance.socketHandle = socketHandle} timeout external pollTimer = do
    inputAvailable <- SysIO.hWaitForInput socketHandle timeout
    if not inputAvailable then
        return $ Right (True, api0{Instance.timeout = Nothing})
    else do
        (dataIn, _, api1) <- recv api0
        dataResult <- pollRequestData api1 external dataIn
        case dataResult of
            Left err ->
                return $ Left err
            Right api2@Instance.T{Instance.timeout = Just result} ->
                return $ Right (result, api2{Instance.timeout = Nothing})
            Right api2 -> do
                (pollTimerNew, timeoutNew) <- if timeout <= 0 then
                        return (0, timeout)
                    else
                        timeoutAdjustment pollTimer timeout
                if timeout == 0 then
                    return $ Right (True, api2{Instance.timeout = Nothing})
                else
                    pollRequestLoop api2 timeoutNew external pollTimerNew

pollRequestLoopBegin :: Typeable.Typeable s =>
    Instance.T s -> Int -> Bool -> Clock.NominalDiffTime ->
    IO (Result (Bool, Instance.T s))
pollRequestLoopBegin api0 timeout external pollTimer = do
    result <- Exception.try (pollRequestLoop api0 timeout external pollTimer)
    case result of
        Left exception ->
            return $ Left $ show (exception :: SomeException)
        Right success ->
            return success

pollRequest :: Typeable.Typeable s =>
    Instance.T s -> Int -> Bool ->
    IO (Result (Bool, Instance.T s))
pollRequest api0@Instance.T{
      Instance.initializationComplete = initializationComplete
    , Instance.terminate = terminate} timeout external =
    if terminate then
        return $ Right (False, api0)
    else do
        pollTimer <- if timeout <= 0 then
                return 0
            else
                POSIX.getPOSIXTime
        if external && not initializationComplete then
            let pollingTerms = Erlang.OtpErlangAtom (Char8.pack "polling") in
            case Erlang.termToBinary pollingTerms (-1) of
                Left err ->
                    return $ Left $ show err
                Right pollingBinary -> do
                    send api0 pollingBinary
                    pollRequestLoopBegin
                        api0{Instance.initializationComplete = True}
                        timeout external pollTimer
        else
            pollRequestLoopBegin api0 timeout external pollTimer

poll :: Typeable.Typeable s =>
    Instance.T s -> Int -> IO (Result (Bool, Instance.T s))
poll api0 timeout =
    pollRequest api0 timeout True

threadList :: Concurrent.MVar [Concurrent.MVar ()]
threadList = Unsafe.unsafePerformIO (Concurrent.newMVar [])

threadCreate :: (Int -> IO ()) -> Int -> IO ThreadId
threadCreate f threadIndex = do
    thread <- Concurrent.newEmptyMVar
    threads <- Concurrent.takeMVar threadList
    Concurrent.putMVar threadList (thread:threads)
    threadCreateFork threadIndex (f threadIndex)
        (\_ -> Concurrent.putMVar thread ())

threadCreateFork :: Int -> IO a -> (Either SomeException a -> IO ()) ->
    IO ThreadId
threadCreateFork threadIndex action afterF =
    -- similar to Concurrent.forkFinally
    Exception.mask $ \restore ->
        Concurrent.forkOn threadIndex
            (Exception.try (restore action) >>= afterF)

threadsWait :: IO ()
threadsWait = do
    threads <- Concurrent.takeMVar threadList
    case threads of
        [] ->
            return ()
        done:remaining -> do
            Concurrent.putMVar threadList remaining
            Concurrent.takeMVar done
            threadsWait

