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

module Main where

import System.Exit (ExitCode(ExitFailure),exitWith)
import qualified Control.Concurrent as Concurrent
import qualified System.IO as SysIO
import qualified Foreign.CloudI as CloudI

task :: Int -> IO ()
task threadIndex = do
    apiValue <- CloudI.api threadIndex ()
    case apiValue of
        Left err -> do
            SysIO.hPutStrLn SysIO.stderr err
        Right api -> do
            pollValue <- CloudI.poll api (-1)
            case pollValue of
                Left err -> do
                    SysIO.hPutStrLn SysIO.stderr err
                Right (True, _) -> do
                    SysIO.hPutStrLn SysIO.stderr "invalid timeout"
                Right (False, _) -> do
                    putStrLn "terminate http_req haskell"

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

