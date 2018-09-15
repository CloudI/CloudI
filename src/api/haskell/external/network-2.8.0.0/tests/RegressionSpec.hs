{-# LANGUAGE OverloadedStrings #-}
-- | Tests for things that didn't work in the past.
module RegressionSpec (main, spec) where

import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "getAddrInfo" $ do
        it "does not cause segfault on macOS 10.8.2 due to AI_NUMERICSERV" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
            void $ getAddrInfo (Just hints) (Just "localhost") Nothing

    describe "Network.Socket.ByteString.recv" $ do
        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            recv sock 1024 `shouldThrow` anyException

    describe "Network.Socket.ByteString.send" $ do
        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            send sock "hello world" `shouldThrow` anyException
