{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Test.Common
  ( -- * Client server configuration
    ClientServer(..)
  , setClientAction
  , setServerAction
  , tcp
  , unix
  , unixWithUnlink
  , udp
  , withPort
  -- * Run a ClientServer configuration
  , test
  , tcpTest
  , udpTest
  -- * Common constants
  , serverAddr
  , serverAddr6
  , unixAddr
  , testMsg
  , lazyTestMsg
  ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket
import System.Directory
import qualified Data.ByteString.Lazy as L
import System.Timeout (timeout)

import Test.Hspec

serverAddr :: String
serverAddr = "127.0.0.1"

serverAddr6 :: String
serverAddr6 = "::1"

testMsg :: ByteString
testMsg = "This is a test message."

lazyTestMsg :: L.ByteString
lazyTestMsg = L.fromStrict "This is a test message."

unixAddr :: String
unixAddr = "/tmp/network-test"

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'unixTest' makes sure that the 'Socket' is
-- closed after the actions have run.
unixWithUnlink :: String -> ((Socket, SockAddr) -> IO b) -> (ClientServer Socket b)
unixWithUnlink address = unix address unlink
  where
    unlink file = do
        exist <- doesFileExist file
        when exist $ removeFile file

unix
    :: String -- ^ address
    -> (String -> IO ()) -- ^ clean up action
    -> ((Socket, SockAddr) -> IO b) -- ^ server action
    -> (ClientServer Socket b)
unix address cleanupAct serverAct = defaultClientServer
    { clientSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix address)
        return sock
    , serverSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        cleanupAct address -- just in case
        bind sock (SockAddrUnix address)
        listen sock 1
        return sock
    , serverAction = \sock ->
        E.bracket (accept sock) (killClientSock . fst) serverAct
    }
  where
    killClientSock sock = do
        shutdown sock ShutdownBoth
        close sock
        cleanupAct address

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'tcpTest' makes sure that the 'Socket' is
-- closed after the actions have run.
tcpTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
tcpTest client server = withPort $ test . setClientAction client . tcp server

tcp :: (Socket -> IO b) -> MVar PortNumber -> ClientServer Socket ()
tcp serverAct portVar = defaultClientServer
    { clientSetup = do
        let hints = defaultHints { addrSocketType = Stream }
        serverPort <- readMVar portVar
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#if !defined(mingw32_HOST_OS)
        withFdSocket sock $ \fd -> do
          getNonBlock fd `shouldReturn` True
          getCloseOnExec fd `shouldReturn` False
#endif
        connect sock $ addrAddress addr
        return sock
    , serverSetup = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        withFdSocket sock $ \fd -> do
#if !defined(mingw32_HOST_OS)
          getNonBlock fd `shouldReturn` True
          getCloseOnExec fd `shouldReturn` False
#endif
          setSocketOption sock ReuseAddr 1
          setCloseOnExecIfNeeded fd
#if !defined(mingw32_HOST_OS)
          getCloseOnExec fd `shouldReturn` True
#endif
        bind sock $ addrAddress addr
        listen sock 1
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock
    , serverAction = \sock -> do
        (clientSock, _) <- accept sock
#if !defined(mingw32_HOST_OS)
        withFdSocket sock $ \fd -> do
          getNonBlock fd `shouldReturn` True
          getCloseOnExec fd `shouldReturn` True
#endif
        _ <- serverAct clientSock
        close clientSock
    }

-- | Create an unconnected 'Socket' for sending UDP and receiving
-- datagrams and then run 'clientAct' and 'serverAct'.
udpTest :: (Socket -> PortNumber -> IO a) -> (Socket -> IO b) -> IO ()
udpTest client server =
    withPort $ test . setServerAction server . udp client

udp
    :: (Socket -> PortNumber -> IO a)
    -> MVar PortNumber
    -> ClientServer a Socket
udp clientAct portVar = defaultClientServer
    { clientSetup = socket AF_INET Datagram defaultProtocol
    , clientAction = \sock -> do
        serverPort <- readMVar portVar
        clientAct sock serverPort
    , serverSetup = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
            , addrSocketType = Datagram
            }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock $ addrAddress addr
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock
    }

data ClientServer a b
    = ClientServer
    { clientSetup :: IO Socket
    , clientAction :: Socket -> IO a
    , serverSetup :: IO Socket
    , serverAction :: Socket -> IO b
    }

setClientAction
    :: (Socket -> IO b)
    -> ClientServer a c
    -> ClientServer b c
setClientAction f c = c { clientAction = f }

setServerAction
    :: (Socket -> IO c)
    -> ClientServer a b
    -> ClientServer a c
setServerAction f c = c { serverAction = f }

defaultClientServer :: ClientServer Socket Socket
defaultClientServer = ClientServer
    { clientSetup =
        E.throwIO $ userError "no client setup defined"
    , clientAction = return
    , serverSetup = E.throwIO $ userError "no server setup defined"
    , serverAction = return
    }

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the 'Socket'.
test :: ClientServer a b -> IO ()
test conf = do
    tid <- myThreadId
    barrier <- newEmptyMVar
    _ <- forkIO $ server tid barrier
    client tid barrier
  where
    server tid barrier =
        bracketWithReraise tid (serverSetup conf) close $ \sock -> do
        serverReady
        Just _ <- timeout 1000000 $ (serverAction conf) sock
        putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()
    client tid barrier = do
        takeMVar barrier
        -- Transfer exceptions to the main thread.
        bracketWithReraise tid (clientSetup conf) close $ \res -> do
            Just _ <- timeout 1000000 $ (clientAction conf) res
            takeMVar barrier

withPort :: (MVar PortNumber -> IO a) -> IO a
withPort f = f =<< newEmptyMVar

-- | Like 'bracket' but catches and reraises the exception in another
-- thread, specified by the first argument.
bracketWithReraise :: ThreadId -> IO a -> (a -> IO b) -> (a -> IO ()) -> IO ()
bracketWithReraise tid setup teardown thing =
    E.bracket setup teardown thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e
