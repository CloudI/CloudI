{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This is the main module of the network package supposed to be
-- used with either "Network.Socket.ByteString" or
-- "Network.Socket.ByteString.Lazy" for sending/receiving.
--
-- Here are two minimal example programs using the TCP/IP protocol: a
-- server that echoes all data that it receives back (servicing only
-- one client) and a client using it.
--
-- > -- Echo server program
-- > module Main (main) where
-- >
-- > import Control.Concurrent (forkFinally)
-- > import qualified Control.Exception as E
-- > import Control.Monad (unless, forever, void)
-- > import qualified Data.ByteString as S
-- > import Network.Socket
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "3000"
-- >     E.bracket (open addr) close loop
-- >   where
-- >     resolve port = do
-- >         let hints = defaultHints {
-- >                 addrFlags = [AI_PASSIVE]
-- >               , addrSocketType = Stream
-- >               }
-- >         addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         setSocketOption sock ReuseAddr 1
-- >         -- If the prefork technique is not used,
-- >         -- set CloseOnExec for the security reasons.
-- >         withFdSocket sock $ setCloseOnExecIfNeeded
-- >         bind sock (addrAddress addr)
-- >         listen sock 10
-- >         return sock
-- >     loop sock = forever $ do
-- >         (conn, peer) <- accept sock
-- >         putStrLn $ "Connection from " ++ show peer
-- >         void $ forkFinally (talk conn) (\_ -> close conn)
-- >     talk conn = do
-- >         msg <- recv conn 1024
-- >         unless (S.null msg) $ do
-- >           sendAll conn msg
-- >           talk conn
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > -- Echo client program
-- > module Main (main) where
-- >
-- > import qualified Control.Exception as E
-- > import qualified Data.ByteString.Char8 as C
-- > import Network.Socket
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "127.0.0.1" "3000"
-- >     E.bracket (open addr) close talk
-- >   where
-- >     resolve host port = do
-- >         let hints = defaultHints { addrSocketType = Stream }
-- >         addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         connect sock $ addrAddress addr
-- >         return sock
-- >     talk sock = do
-- >         sendAll sock "Hello, world!"
-- >         msg <- recv sock 1024
-- >         putStr "Received: "
-- >         C.putStrLn msg
--
-- The proper programming model is that one 'Socket' is handled by
-- a single thread. If multiple threads use one 'Socket' concurrently,
-- unexpected things would happen. There is one exception for multiple
-- threads vs a single 'Socket': one thread reads data from a 'Socket'
-- only and the other thread writes data to the 'Socket' only.
-----------------------------------------------------------------------------

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Initialisation
      withSocketsDo

    -- * Address information
    , getAddrInfo
    -- ** Types
    , HostName
    , ServiceName
    , AddrInfo(..)
    , defaultHints
    -- ** Flags
    , AddrInfoFlag(..)
    , addrInfoFlagImplemented

    -- * Socket operations
    , connect
    , bind
    , listen
    , accept
    -- ** Closing
    , close
    , close'
    , shutdown
    , ShutdownCmd(..)

    -- * Socket options
    , SocketOption(..)
    , isSupportedSocketOption
    , getSocketOption
    , setSocketOption

    -- * Socket
    , Socket
    , socket
    , fdSocket
    , withFdSocket
    , mkSocket
    , socketToHandle
    -- ** Types of Socket
    , SocketType(..)
    , isSupportedSocketType
    , getSocketType
    -- ** Family
    , Family(..)
    , isSupportedFamily
    , packFamily
    , unpackFamily
    -- ** Protocol number
    , ProtocolNumber
    , defaultProtocol
    -- * Basic socket address type
    , SockAddr(..)
    , isSupportedSockAddr
    , getPeerName
    , getSocketName
    -- ** Host address
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
    -- ** Host address6
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    -- ** Flow Info
    , FlowInfo
    -- ** Scope ID
    , ScopeID
    , ifNameToIndex
    , ifIndexToName
    -- ** Port number
    , PortNumber
    , defaultPort
    , socketPortSafe
    , socketPort

    -- * UNIX-domain socket
    , isUnixDomainSocketAvailable
    , socketPair
    , sendFd
    , recvFd
    , getPeerCredential

    -- * Name information
    , getNameInfo
    , NameInfoFlag(..)

    -- * Low level
    -- ** socket operations
    , setCloseOnExecIfNeeded
    , getCloseOnExec
    , setNonBlockIfNeeded
    , getNonBlock
    -- ** Sending and receiving data
    , sendBuf
    , recvBuf
    , sendBufTo
    , recvBufFrom

    -- * Special constants
    , maxListenQueue
    ) where

import Network.Socket.Buffer hiding (sendBufTo, recvBufFrom)
import Network.Socket.Cbits
import Network.Socket.Fcntl
import Network.Socket.Handle
import Network.Socket.If
import Network.Socket.Info
import Network.Socket.Internal
import Network.Socket.Name hiding (getPeerName, getSocketName)
import Network.Socket.Options
import Network.Socket.Shutdown
import Network.Socket.SockAddr
import Network.Socket.Syscall hiding (connect, bind, accept)
import Network.Socket.Types
import Network.Socket.Unix
