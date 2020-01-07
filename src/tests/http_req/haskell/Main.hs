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

module Main where

import System.Exit (ExitCode(ExitFailure),exitWith)
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Foreign.CloudI as CloudI
import qualified System.IO as SysIO
type ByteString = ByteString.ByteString
type RequestType = CloudI.RequestType
type Source = CloudI.Source

request_ :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    () -> CloudI.T () -> IO (CloudI.Response ())
request_ requestType name pattern _ request timeout _ transId pid state api =
    let httpQs = CloudI.infoKeyValueParse request 
        value = Map.lookup (Char8.pack "value") httpQs >>=
            (\l -> Just (read (Char8.unpack $ head l) :: Integer))
        response = Char8.pack $ case value of
            Nothing ->
                "<http_test><error>no value specified</error></http_test>"
            Just (i) ->
                "<http_test><value>" ++ (show i) ++ "</value></http_test>"
    in do
    CloudI.return_ api
        requestType name pattern ByteString.empty response timeout transId pid
    return $ CloudI.Null (state, api)

task :: Int -> IO ()
task threadIndex = do
    let prerr = SysIO.hPutStrLn SysIO.stderr
        prout = putStrLn
    apiValue <- CloudI.api threadIndex () Nothing
    case apiValue of
        Left err ->
            prerr err
        Right api0 -> do
            let suffix = Char8.pack "haskell.xml/get"
            countValue0 <- CloudI.subscribeCount api0 suffix
            case countValue0 of
                Left err ->
                    prerr err
                Right (0, api1) -> do
                    subscribeValue <- CloudI.subscribe api1 suffix request_
                    case subscribeValue of
                        Left err ->
                            prerr err
                        Right api2 -> do
                            countValue1 <- CloudI.subscribeCount api2 suffix
                            case countValue1 of
                                Left err ->
                                    prerr err
                                Right (1, api3) -> do
                                    pollValue <- CloudI.poll api3 (-1)
                                    case pollValue of
                                        Left err ->
                                            prerr err
                                        Right (_, _) ->
                                            prout "terminate http_req haskell"
                                Right (_, _) ->
                                    prerr "subscribe_count /= 1"
                Right (_, _) ->
                    prerr "subscribe_count /= 0"

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

