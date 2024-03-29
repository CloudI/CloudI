{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  MIT License

  Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>

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

module Main where

import Control.Exception (assert)
import System.Exit (ExitCode(ExitFailure),exitWith)
import qualified Control.Concurrent as Concurrent
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Foreign.CloudI as CloudI
import qualified System.IO as SysIO
type ByteString = ByteString.ByteString
type RequestType = CloudI.RequestType
type Source = CloudI.Source

return_ :: CloudI.T () -> RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> ByteString -> Source ->
    IO (CloudI.Response ())
return_ api0 requestType name pattern
    responseInfo response timeout transId source = do
    CloudI.return_ api0
        requestType name pattern responseInfo response timeout transId source
    return $ CloudI.Null ((), api0)

empty :: ByteString
empty = ByteString.empty

sequence1ABCD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1ABCD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/b/c/d")))
    assert (request == Char8.pack "test1")
    return_ api requestType name pattern empty request timeout transId source

sequence1ABCX :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1ABCX requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/b/c/*")))
    assert (request == Char8.pack "test2" || request == Char8.pack "test3")
    return_ api requestType name pattern empty request timeout transId source

sequence1ABXD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1ABXD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/b/*/d")))
    assert (request == Char8.pack "test4" || request == Char8.pack "test5")
    return_ api requestType name pattern empty request timeout transId source

sequence1AXCD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1AXCD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/*/c/d")))
    assert (request == Char8.pack "test6" || request == Char8.pack "test7")
    return_ api requestType name pattern empty request timeout transId source

sequence1XBCD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1XBCD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "*/b/c/d")))
    assert (request == Char8.pack "test8" || request == Char8.pack "test9")
    return_ api requestType name pattern empty request timeout transId source

sequence1ABX :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1ABX requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/b/*")))
    assert (request == Char8.pack "test10")
    return_ api requestType name pattern empty request timeout transId source

sequence1AXD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1AXD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/*/d")))
    assert (request == Char8.pack "test11")
    return_ api requestType name pattern empty request timeout transId source

sequence1XCD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1XCD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "*/c/d")))
    assert (request == Char8.pack "test12")
    return_ api requestType name pattern empty request timeout transId source

sequence1AX :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1AX requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "a/*")))
    assert (request == Char8.pack "test13")
    return_ api requestType name pattern empty request timeout transId source

sequence1XD :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1XD requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "*/d")))
    assert (request == Char8.pack "test14")
    return_ api requestType name pattern empty request timeout transId source

sequence1X :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1X requestType name pattern
    _ request timeout _ transId source _ api =
    assert (pattern ==
        (ByteString.append (CloudI.prefix api) (Char8.pack "*")))
    assert (request == Char8.pack "test15")
    return_ api requestType name pattern empty request timeout transId source

sendAsync :: CloudI.T () -> String -> String -> IO (ByteString, CloudI.T ())
sendAsync api0 suffix request = do
    result <- CloudI.sendAsync api0
        (ByteString.append (CloudI.prefix api0) (Char8.pack suffix))
        (Char8.pack request) Nothing Nothing Nothing
    case result of
        Left err ->
            error err
        Right (transId, api1) ->
            return (transId, api1)

sequence1 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence1 requestType name pattern _ request timeout _ transId source _ api0 =
    let wait api1 = do
            waitValue <- CloudI.recvAsync api1 (Just 1000) Nothing Nothing
            case waitValue of
                Left err ->
                    error err
                Right (_, responseWaited, _, api2) ->
                    if responseWaited == Char8.pack "end" then
                        wait api2
                    else
                        return api2
    in do
    api3 <- wait api0
    let iteration = read $ Char8.unpack request :: Int
        requestNew = show iteration
    putStrLn $ "messaging sequence1 start haskell (" ++ requestNew ++ ")"
    (test1Id, api4) <- sendAsync api3 "a/b/c/d" "test1"
    (test2Id, api5) <- sendAsync api4 "a/b/c/z" "test2"
    (test3Id, api6) <- sendAsync api5 "a/b/c/dd" "test3"
    (test4Id, api7) <- sendAsync api6 "a/b/z/d" "test4"
    (test5Id, api8) <- sendAsync api7 "a/b/cc/d" "test5"
    (test6Id, api9) <- sendAsync api8 "a/z/c/d" "test6"
    (test7Id, api10) <- sendAsync api9 "a/bb/c/d" "test7"
    (test8Id, api11) <- sendAsync api10 "z/b/c/d" "test8"
    (test9Id, api12) <- sendAsync api11 "aa/b/c/d" "test9"
    (test10Id, api13) <- sendAsync api12 "a/b/czd" "test10"
    (test11Id, api14) <- sendAsync api13 "a/bzc/d" "test11"
    (test12Id, api15) <- sendAsync api14 "azb/c/d" "test12"
    (test13Id, api16) <- sendAsync api15 "a/bzczd" "test13"
    (test14Id, api17) <- sendAsync api16 "azbzc/d" "test14"
    (test15Id, api18) <- sendAsync api17 "azbzczd" "test15"
    {- n.b., depends on cloudi_core_i_constants.hrl having
       RECV_ASYNC_STRATEGY == recv_async_select_oldest -}
    let recvAsyncWait api19 transIdWait = do
            recvAsyncWaitValue <- CloudI.recvAsync api19
                Nothing (Just transIdWait) (Just False)
            case recvAsyncWaitValue of
                Left err ->
                    error err
                Right (_, _, transIdWaited, api20) ->
                    if transIdWait == transIdWaited then
                        return api20
                    else
                        error "timeout!"
        recvAsyncAssert api21 transIdAssert responseAssert = do
            recvAsyncAssertValue <- CloudI.recvAsync api21
                Nothing Nothing Nothing
            case recvAsyncAssertValue of
                Left err ->
                    error err
                Right (_, responseAsserted, transIdAsserted, api22) ->
                    if transIdAssert == transIdAsserted then
                        let _ = assert
                                (responseAssert == responseAsserted) () in
                        return api22
                    else
                        error "timeout!"
    api23 <- recvAsyncWait api18 test1Id
    api24 <- recvAsyncAssert api23 test1Id (Char8.pack "test1")
    api25 <- recvAsyncWait api24 test2Id
    api26 <- recvAsyncAssert api25 test2Id (Char8.pack "test2")
    api27 <- recvAsyncWait api26 test3Id
    api28 <- recvAsyncAssert api27 test3Id (Char8.pack "test3")
    api29 <- recvAsyncWait api28 test4Id
    api30 <- recvAsyncAssert api29 test4Id (Char8.pack "test4")
    api31 <- recvAsyncWait api30 test5Id
    api32 <- recvAsyncAssert api31 test5Id (Char8.pack "test5")
    api33 <- recvAsyncWait api32 test6Id
    api34 <- recvAsyncAssert api33 test6Id (Char8.pack "test6")
    api35 <- recvAsyncWait api34 test7Id
    api36 <- recvAsyncAssert api35 test7Id (Char8.pack "test7")
    api37 <- recvAsyncWait api36 test8Id
    api38 <- recvAsyncAssert api37 test8Id (Char8.pack "test8")
    api39 <- recvAsyncWait api38 test9Id
    api40 <- recvAsyncAssert api39 test9Id (Char8.pack "test9")
    api41 <- recvAsyncWait api40 test10Id
    api42 <- recvAsyncAssert api41 test10Id (Char8.pack "test10")
    api43 <- recvAsyncWait api42 test11Id
    api44 <- recvAsyncAssert api43 test11Id (Char8.pack "test11")
    api45 <- recvAsyncWait api44 test12Id
    api46 <- recvAsyncAssert api45 test12Id (Char8.pack "test12")
    api47 <- recvAsyncWait api46 test13Id
    api48 <- recvAsyncAssert api47 test13Id (Char8.pack "test13")
    api49 <- recvAsyncWait api48 test14Id
    api50 <- recvAsyncAssert api49 test14Id (Char8.pack "test14")
    api51 <- recvAsyncWait api50 test15Id
    api52 <- recvAsyncAssert api51 test15Id (Char8.pack "test15")
    putStrLn $ "messaging sequence1 end haskell (" ++ requestNew ++ ")"
    (_, api53) <- sendAsync api52 "sequence2" requestNew
    let response = Char8.pack "end"
    return_ api53 requestType name pattern
        empty response timeout transId source

sequence2E1 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E1 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "1" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E2 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E2 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "2" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E3 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E3 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "3" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E4 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E4 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "4" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E5 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E5 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "5" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E6 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E6 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "6" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E7 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E7 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "7" in
    return_ api requestType name pattern empty response timeout transId source

sequence2E8 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2E8 requestType name pattern _ _ timeout _ transId source _ api =
    let response = Char8.pack "8" in
    return_ api requestType name pattern empty response timeout transId source

sequence2 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence2 requestType name pattern
    _ request timeout _ transId source _ api0 = do
    let iteration = read $ Char8.unpack request :: Int
        requestNew = show iteration
    putStrLn $ "messaging sequence2 start haskell (" ++ requestNew ++ ")"
    let recvAsyncsLoop api1 = do
            {- the sending process is excluded from the services that receive
               the asynchronous message, so in this case, the receiving thread
               will not be called, despite the fact it has subscribed to 'e',
               to prevent a process (in this case thread) from deadlocking
               with itself. -}
            mcastAsyncValue <- CloudI.mcastAsync api1
                (ByteString.append (CloudI.prefix api1) (Char8.pack "e"))
                (Char8.pack " ") Nothing Nothing Nothing
            case mcastAsyncValue of
                Left err ->
                    error err
                Right (transIds, api2) ->
                    let (iFirst, iLast) = IArray.bounds transIds
                        loop i l api3 = do
                            let transIdRecv = (IArray.!) transIds i
                            recvAsyncValue <- CloudI.recvAsync api3
                                Nothing (Just transIdRecv) Nothing
                            case recvAsyncValue of
                                Left err ->
                                    error err
                                Right (_, j, transIdLoop, api4) ->
                                    if transIdRecv == transIdLoop then
                                        if i == iLast then
                                            return (j:l, api4)
                                        else
                                            loop (i + 1) (j:l) api4
                                    else
                                        error "timeout!"
                    in do
                    (eCheckList, api5) <- loop iFirst [] api2
                    {- 4 * 8 == 32, but only 3 out of 4 threads
                       can receive messages, since 1 thread is sending
                       the mcast_async, so 3 * 8 == 24 -}
                    if List.length eCheckList == 24 then
                        assert ((ByteString.concat $ List.sort eCheckList) ==
                            Char8.pack "111222333444555666777888")
                        return api5
                    else
                        let transIdsLength = fromIntegral
                                (iLast - iFirst + 1) :: Double
                            count = 4.0 - transIdsLength / 8.0 in do
                        putStrLn $ "Waiting for " ++ (show count) ++
                            " services to initialize"
                        recvAsyncValue <- CloudI.recvAsync api5
                            (Just 1000) Nothing Nothing
                        case recvAsyncValue of
                            Left err ->
                                error err
                            Right (_, _, transIdWait, api6) ->
                                if CloudI.transIdNull == transIdWait then
                                    recvAsyncsLoop api6
                                else
                                    error "invalid!"
    api7 <- recvAsyncsLoop api0
    putStrLn $ "messaging sequence2 end haskell (" ++ requestNew ++ ")"
    (_, api8) <- sendAsync api7 "sequence3" requestNew
    let response = Char8.pack "end"
    return_ api8 requestType name pattern empty response timeout transId source

sequence3F1 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence3F1 requestType _ _
    requestInfo request timeout priority transId source _ api =
    let requestI = read $ Char8.unpack request :: Int in
    if requestI == 4 then
        return $ CloudI.Response (Char8.pack "done", (), api)
    else
        let requestNew = requestI + 2 in do -- 2 steps forward
        CloudI.forward_ api requestType
            (ByteString.append (CloudI.prefix api) (Char8.pack "f2"))
            requestInfo (Char8.pack $ show requestNew)
            timeout priority transId source
        return $ CloudI.Null ((), api)

sequence3F2 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence3F2 requestType _ _
    requestInfo request timeout priority transId source _ api =
    let requestI = read $ Char8.unpack request :: Int
        requestNew = requestI - 1 in do -- 1 step back
    CloudI.forward_ api requestType
        (ByteString.append (CloudI.prefix api) (Char8.pack "f1"))
        requestInfo (Char8.pack $ show requestNew)
        timeout priority transId source
    return $ CloudI.Null ((), api)

sequence3G1 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence3G1 requestType name pattern _ request timeout _ transId source _ api =
    let response = ByteString.append request $ Char8.pack "suffix" in
    return_ api requestType name pattern empty response timeout transId source

sequence3 :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
sequence3 requestType name pattern
    _ request timeout _ transId source _ api0 = do
    let iteration = read $ Char8.unpack request :: Int
        requestOld = show iteration
    putStrLn $ "messaging sequence3 start haskell (" ++ requestOld ++ ")"
    (test1Id, api1) <- sendAsync api0 "f1" "0"
    recvAsyncValue <- CloudI.recvAsync api1 Nothing (Just test1Id) Nothing
    case recvAsyncValue of
        Left err ->
            error err
        Right (_, test1Check, test1IdCheck, api2) -> do
            let _ = assert (test1IdCheck == test1Id)
                    assert (test1Check == Char8.pack "done") ()
            sendSyncValue <- CloudI.sendSync api2
                (ByteString.append (CloudI.prefix api2) (Char8.pack "g1"))
                (Char8.pack "prefix_") Nothing Nothing Nothing
            case sendSyncValue of
                Left err ->
                    error err
                Right (_, test2Check, _, api3) -> do
                    let _ = assert (test2Check == Char8.pack "prefix_suffix") ()
                    putStrLn $ "messaging sequence3 end haskell (" ++
                        requestOld ++ ")"
                    let iterationNext = iteration + 1
                        iterationNew =
                            if iterationNext == maxBound then
                                0
                            else
                                iterationNext
                    (_, api4) <- sendAsync api3 "sequence1" (show iterationNew)
                    let response = Char8.pack "end"
                    return_ api4
                        requestType name pattern empty response
                        timeout transId source

task :: Int -> IO ()
task threadIndex = do
    let prerr = SysIO.hPutStrLn SysIO.stderr
        prout = putStrLn
    apiValue <- CloudI.api threadIndex () (Just False)
    case apiValue of
        Left err ->
            prerr err
        Right api0 ->
            let subscribes ((s, f):l) api1 = do
                    subscribeValue <- CloudI.subscribe api1 (Char8.pack s) f
                    case subscribeValue of
                        Left _ ->
                            return subscribeValue
                        Right api2 ->
                            subscribes l api2
                subscribes [] api1 =
                    return $ Right api1
                listF = 
                    [ ("a/b/c/d", sequence1ABCD)
                    , ("a/b/c/*", sequence1ABCX)
                    , ("a/b/*/d", sequence1ABXD)
                    , ("a/*/c/d", sequence1AXCD)
                    , ("*/b/c/d", sequence1XBCD)
                    , ("a/b/*",   sequence1ABX)
                    , ("a/*/d",   sequence1AXD)
                    , ("*/c/d",   sequence1XCD)
                    , ("a/*",     sequence1AX)
                    , ("*/d",     sequence1XD)
                    , ("*",       sequence1X)
                    , ("sequence1", sequence1)
                    , ("e", sequence2E1)
                    , ("e", sequence2E2)
                    , ("e", sequence2E3)
                    , ("e", sequence2E4)
                    , ("e", sequence2E5)
                    , ("e", sequence2E6)
                    , ("e", sequence2E7)
                    , ("e", sequence2E8)
                    , ("sequence2", sequence2)
                    , ("f1", sequence3F1)
                    , ("f2", sequence3F2)
                    , ("g1", sequence3G1)
                    , ("sequence3", sequence3)
                    ]
            in do
            subscribesValue <- subscribes listF api0
            case subscribesValue of
                Left err ->
                    prerr err
                Right api3 -> do
                    api5 <- if threadIndex == 0 then do
                            (_, api4) <- sendAsync api3 "sequence1" "1"
                            return api4
                        else
                            return api3
                    pollValue <- CloudI.poll api5 (-1)
                    case pollValue of
                        Left err ->
                            prerr err
                        Right (_, _) ->
                            prout "terminate messaging haskell"

main :: IO ()
main = do
    threadCountValue <- CloudI.threadCount
    case threadCountValue of
        Left err -> do
            SysIO.hPutStrLn SysIO.stderr err
            _ <- exitWith (ExitFailure 1)
            return ()
        Right threadCount -> do
            Concurrent.setNumCapabilities threadCount
            mapM_ (CloudI.threadCreate task) [0..threadCount - 1]
            CloudI.threadsWait

