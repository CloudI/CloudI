-- | Tests for things that didn't work in the past.
module Main where

import Network.Socket

import Control.Exception

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertFailure)

import qualified Regression.Issue215 as Issue215

------------------------------------------------------------------------
-- Tests

-- Used to segfault on OS X 10.8.2 due to AI_NUMERICSERV being set
-- without a service being set. This is a OS X bug.
testGetAddrInfo :: IO ()
testGetAddrInfo = do
    let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
    _ <- getAddrInfo (Just hints) (Just "localhost") Nothing
    return ()

mkBadSocketAndTry :: (Socket -> IO a) -> IO (Either IOException a)
mkBadSocketAndTry f = do
    sock <- socket AF_INET Stream defaultProtocol
    try $ f sock

-- Because of 64/32 bitness issues, -1 wasn't correctly checked for on Windows.
-- See also GHC ticket #12010
badRecvShouldThrow :: IO ()
badRecvShouldThrow = do
    res <- mkBadSocketAndTry $ flip recv 1024
    case res of
        Left _ex -> return ()
        Right _  -> assertFailure "recv didn't throw an exception"

badSendShouldThrow :: IO ()
badSendShouldThrow = do
    res <- mkBadSocketAndTry $ flip send "hello"
    case res of
        Left _ex -> return ()
        Right _  -> assertFailure "send didn't throw an exception"

------------------------------------------------------------------------
-- List of all tests

tests :: [Test]
tests =
    [ testCase "testGetAddrInfo" testGetAddrInfo
    , testCase "badRecvShouldThrow" badRecvShouldThrow
    , testCase "badSendShouldThrow" badSendShouldThrow
    , testCase "recvShouldntThrowOnClosedSocket" Issue215.main
    ]

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ defaultMain tests
