{--*-Mode:haskell;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=haskell fenc=utf-8 sts=4 ts=4 sw=4 et nomod: -}

{-

  MIT License

  Copyright (c) 2017 Michael Truog <mjtruog at gmail dot com>

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

import Data.Typeable (Typeable)
import System.Exit (ExitCode(ExitFailure),exitWith)
import qualified Control.Concurrent as Concurrent
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign.CloudI as CloudI
import qualified System.IO as SysIO
type ByteString = ByteString.ByteString
type RequestType = CloudI.RequestType
type Source = CloudI.Source

data CountState = State
    { count_value :: !Int
    }
    deriving (Typeable)

request_ :: RequestType -> ByteString -> ByteString ->
    ByteString -> ByteString -> Int -> Int -> ByteString -> Source ->
    CountState -> CloudI.T CountState -> IO (CloudI.Response CountState)
request_ _ _ _ _ _ _ _ _ _ state@State{count_value = count0} api = do
    let count1 = if count0 == 4294967295 then 0 else count0 + 1
        count_str = show count1
    putStrLn $ "count == " ++ count_str ++ " haskell"
    let response = Char8.pack count_str
    return $ CloudI.Response (response, state{count_value = count1}, api)

task :: Int -> IO ()
task threadIndex = do
    let prerr = SysIO.hPutStrLn SysIO.stderr
        prout = putStrLn
    apiValue <- CloudI.api threadIndex State{count_value = 0}
    case apiValue of
        Left err ->
            prerr err
        Right api0 -> do
            let suffix = Char8.pack "haskell/get"
            subscribeValue <- CloudI.subscribe api0 suffix request_
            case subscribeValue of
                Left err ->
                    prerr err
                Right api1 -> do
                    pollValue <- CloudI.poll api1 (-1)
                    case pollValue of
                        Left err ->
                            prerr err
                        Right (_, _) ->
                            prout "terminate count haskell"

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

