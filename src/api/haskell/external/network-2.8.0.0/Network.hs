{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module is kept for backwards-compatibility. New users are
-- encouraged to use "Network.Socket" instead.
--
-- "Network" was intended as a \"higher-level\" interface to networking
-- facilities, and only supports TCP.
--
-----------------------------------------------------------------------------

#include "HsNetworkConfig.h"

#ifdef HAVE_GETADDRINFO
-- Use IPv6-capable function definitions if the OS supports it.
#define IPV6_SOCKET_SUPPORT 1
#endif

module Network {-# DEPRECATED "The high level Network interface is no longer supported. Please use Network.Socket." #-}
    (
    -- * Basic data types
      Socket
    , PortID(..)
    , HostName
    , PortNumber

    -- * Initialisation
    , withSocketsDo

    -- * Server-side connections
    , listenOn
    , accept
    , sClose

    -- * Client-side connections
    , connectTo

    -- * Simple sending and receiving
    {-$sendrecv-}
    , sendTo
    , recvFrom

    -- * Miscellaneous
    , socketPort

    -- * Networking Issues
    -- ** Buffering
    {-$buffering-}

    -- ** Improving I\/O Performance over sockets
    {-$performance-}
    ) where

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Network.BSD
import Network.Socket hiding (accept, socketPort, recvFrom,
                              sendTo, PortNumber, sClose)
import qualified Network.Socket as Socket (accept)
import System.IO
import Prelude
import qualified Control.Exception as Exception

-- ---------------------------------------------------------------------------
-- High Level ``Setup'' functions

-- If the @PortID@ specifies a unix family socket and the @Hostname@
-- differs from that returned by @getHostname@ then an error is
-- raised. Alternatively an empty string may be given to @connectTo@
-- signalling that the current hostname applies.

data PortID =
          Service String                -- Service Name eg "ftp"
        | PortNumber PortNumber         -- User defined Port Number
#if !defined(mingw32_HOST_OS)
        | UnixSocket String             -- Unix family socket in file system
#endif
        deriving (Show, Eq)

-- | Calling 'connectTo' creates a client side socket which is
-- connected to the given host and port.  The Protocol and socket type is
-- derived from the given port identifier.  If a port number is given
-- then the result is always an internet family 'Stream' socket.

connectTo :: HostName           -- Hostname
          -> PortID             -- Port Identifier
          -> IO Handle          -- Connected Socket

#if defined(IPV6_SOCKET_SUPPORT)
-- IPv6 and IPv4.

connectTo hostname (Service serv) = connect' "Network.connectTo" hostname serv

connectTo hostname (PortNumber port) = connect' "Network.connectTo" hostname (show port)
#else
-- IPv4 only.

connectTo hostname (Service serv) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)  -- only done if there's an error
        (\sock -> do
          port  <- getServicePortNumber serv
          he    <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          socketToHandle sock ReadWriteMode
        )

connectTo hostname (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)  -- only done if there's an error
        (\sock -> do
          he <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          socketToHandle sock ReadWriteMode
        )
#endif

#if !defined(mingw32_HOST_OS)
connectTo _ (UnixSocket path) = do
    bracketOnError
        (socket AF_UNIX Stream 0)
        (sClose)
        (\sock -> do
          connect sock (SockAddrUnix path)
          socketToHandle sock ReadWriteMode
        )
#endif

#if defined(IPV6_SOCKET_SUPPORT)
connect' :: String -> HostName -> ServiceName -> IO Handle

connect' caller host serv = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
    firstSuccessful caller $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        (sClose)  -- only done if there's an error
        (\sock -> do
          connect sock (addrAddress addr)
          socketToHandle sock ReadWriteMode
        )
#endif

-- | Creates the server side socket which has been bound to the
-- specified port.
--
-- 'maxListenQueue' (typically 128) is specified to the listen queue.
-- This is good enough for normal network servers but is too small
-- for high performance servers.
--
-- To avoid the \"Address already in use\" problems,
-- the 'ReuseAddr' socket option is set on the listening socket.
--
-- If available, the 'IPv6Only' socket option is set to 0
-- so that both IPv4 and IPv6 can be accepted with this socket.
--
-- If you don't like the behavior above, please use the lower level
-- 'Network.Socket.listen' instead.

listenOn :: PortID      -- ^ Port Identifier
         -> IO Socket   -- ^ Listening Socket

#if defined(IPV6_SOCKET_SUPPORT)
-- IPv6 and IPv4.

listenOn (Service serv) = listen' serv

listenOn (PortNumber port) = listen' (show port)
#else
-- IPv4 only.

listenOn (Service serv) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            port    <- getServicePortNumber serv
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrInet port iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )

listenOn (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrInet port iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )
#endif

#if !defined(mingw32_HOST_OS)
listenOn (UnixSocket path) =
    bracketOnError
        (socket AF_UNIX Stream 0)
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrUnix path)
            listen sock maxListenQueue
            return sock
        )
#endif

#if defined(IPV6_SOCKET_SUPPORT)
listen' :: ServiceName -> IO Socket

listen' serv = do
    proto <- getProtocolNumber "tcp"
    -- We should probably specify addrFamily = AF_INET6 and the filter
    -- code below should be removed. AI_ADDRCONFIG is probably not
    -- necessary. But this code is well-tested. So, let's keep it.
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                             , addrSocketType = Stream
                             , addrProtocol = proto }
    addrs <- getAddrInfo (Just hints) Nothing (Just serv)
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = if null addrs' then head addrs else head addrs'
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )
#endif

-- -----------------------------------------------------------------------------
-- accept

-- | Accept a connection on a socket created by 'listenOn'.  Normal
-- I\/O operations (see "System.IO") can be used on the 'Handle'
-- returned to communicate with the client.
-- Notice that although you can pass any Socket to Network.accept,
-- only sockets of either AF_UNIX, AF_INET, or AF_INET6 will work
-- (this shouldn't be a problem, though). When using AF_UNIX, HostName
-- will be set to the path of the socket and PortNumber to -1.
--
accept :: Socket                -- ^ Listening Socket
       -> IO (Handle,
              HostName,
              PortNumber)       -- ^ Triple of: read\/write 'Handle' for
                                -- communicating with the client,
                                -- the 'HostName' of the peer socket, and
                                -- the 'PortNumber' of the remote connection.
accept sock@(MkSocket _ AF_INET _ _ _) = do
 ~(sock', (SockAddrInet port haddr)) <- Socket.accept sock
 peer <- catchIO
          (do
             (HostEntry peer _ _ _) <- getHostByAddr AF_INET haddr
             return peer
          )
          (\_e -> inet_ntoa haddr)
                -- if getHostByName fails, we fall back to the IP address
 handle <- socketToHandle sock' ReadWriteMode
 return (handle, peer, port)
#if defined(IPV6_SOCKET_SUPPORT)
accept sock@(MkSocket _ AF_INET6 _ _ _) = do
 (sock', addr) <- Socket.accept sock
 peer <- catchIO ((fromJust . fst) `liftM` getNameInfo [] True False addr) $
         \_ -> case addr of
                 SockAddrInet  _   a   -> inet_ntoa a
                 SockAddrInet6 _ _ a _ -> return (show a)
#if defined(mingw32_HOST_OS)
                 SockAddrUnix {}       -> ioError $ userError "Network.accept: peer socket address 'SockAddrUnix' not supported on this platform."
#else
                 SockAddrUnix      a   -> return a
#endif
#if defined(CAN_SOCKET_SUPPORT)
                 SockAddrCan {}        -> ioError $ userError "Network.accept: peer socket address 'SockAddrCan' not supported."
#else
                 SockAddrCan {}        -> ioError $ userError "Network.accept: peer socket address 'SockAddrCan' not supported on this platform."
#endif
 handle <- socketToHandle sock' ReadWriteMode
 let port = case addr of
              SockAddrInet  p _     -> p
              SockAddrInet6 p _ _ _ -> p
              _                     -> -1
 return (handle, peer, port)
#endif
#if !defined(mingw32_HOST_OS)
accept sock@(MkSocket _ AF_UNIX _ _ _) = do
 ~(sock', (SockAddrUnix path)) <- Socket.accept sock
 handle <- socketToHandle sock' ReadWriteMode
 return (handle, path, -1)
#endif
accept (MkSocket _ family _ _ _) =
  ioError $ userError $ "Network.accept: address family '" ++
    show family ++ "' not supported."


-- | Close the socket. Sending data to or receiving data from closed socket
-- may lead to undefined behaviour.
sClose :: Socket -> IO ()
sClose = close -- Explicit redefinition because Network.sClose is deprecated,
               -- hence the re-export would also be marked as such.

-- -----------------------------------------------------------------------------
-- sendTo/recvFrom

{-$sendrecv
Send and receive data from\/to the given host and port number.  These
should normally only be used where the socket will not be required for
further calls. Also, note that due to the use of 'hGetContents' in 'recvFrom'
the socket will remain open (i.e. not available) even if the function already
returned. Their use is strongly discouraged except for small test-applications
or invocations from the command line.
-}

sendTo :: HostName      -- Hostname
       -> PortID        -- Port Number
       -> String        -- Message to send
       -> IO ()
sendTo h p msg = do
  s <- connectTo h p
  hPutStr s msg
  hClose s

recvFrom :: HostName    -- Hostname
         -> PortID      -- Port Number
         -> IO String   -- Received Data

#if defined(IPV6_SOCKET_SUPPORT)
recvFrom host port = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    allowed <- map addrAddress `liftM` getAddrInfo (Just hints) (Just host)
                                                   Nothing
    s <- listenOn port
    let waiting = do
        (s', addr) <- Socket.accept s
        if not (addr `oneOf` allowed)
         then sClose s' >> waiting
         else socketToHandle s' ReadMode >>= hGetContents
    waiting
  where
    a@(SockAddrInet _ ha) `oneOf` ((SockAddrInet _ hb):bs)
        | ha == hb = True
        | otherwise = a `oneOf` bs
    a@(SockAddrInet6 _ _ ha _) `oneOf` ((SockAddrInet6 _ _ hb _):bs)
        | ha == hb = True
        | otherwise = a `oneOf` bs
    _ `oneOf` _ = False
#else
recvFrom host port = do
 ip  <- getHostByName host
 let ipHs = hostAddresses ip
 s   <- listenOn port
 let
  waiting = do
     ~(s', SockAddrInet _ haddr)  <-  Socket.accept s
     he <- getHostByAddr AF_INET haddr
     if not (any (`elem` ipHs) (hostAddresses he))
      then do
         sClose s'
         waiting
      else do
        h <- socketToHandle s' ReadMode
        msg <- hGetContents h
        return msg

 message <- waiting
 return message
#endif

-- ---------------------------------------------------------------------------
-- Access function returning the port type/id of socket.

-- | Returns the 'PortID' associated with a given socket.
socketPort :: Socket -> IO PortID
socketPort s = do
    sockaddr <- getSocketName s
    case sockaddr of
      SockAddrInet port _      -> return $ PortNumber port
#if defined(IPV6_SOCKET_SUPPORT)
      SockAddrInet6 port _ _ _ -> return $ PortNumber port
#else
      SockAddrInet6 {}         -> ioError $ userError "Network.socketPort: socket address 'SockAddrInet6' not supported on this platform."
#endif
#if defined(mingw32_HOST_OS)
      SockAddrUnix {}          -> ioError $ userError "Network.socketPort: socket address 'SockAddrUnix' not supported on this platform."
#else
      SockAddrUnix path        -> return $ UnixSocket path
#endif
      SockAddrCan {}           -> ioError $ userError "Network.socketPort: socket address 'SockAddrCan' not supported."

-- ---------------------------------------------------------------------------
-- Utils

-- Like bracket, but only performs the final action if there was an
-- exception raised by the middle bit.
bracketOnError
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracketOnError = Exception.bracketOnError

-----------------------------------------------------------------------------
-- Extra documentation

{-$buffering

The 'Handle' returned by 'connectTo' and 'accept' is 'NoBuffering' by
default.  For an interactive application you may want to set the
buffering mode on the 'Handle' to
'LineBuffering' or 'BlockBuffering', like so:

> h <- connectTo host port
> hSetBuffering h LineBuffering
-}

{-$performance

For really fast I\/O, it might be worth looking at the 'hGetBuf' and
'hPutBuf' family of functions in "System.IO".
-}

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#if MIN_VERSION_base(4,0,0)
catchIO = Exception.catch
#else
catchIO = Exception.catchJust Exception.ioErrors
#endif

-- Version of try implemented in terms of the locally defined catchIO
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO m = catchIO (liftM Right m) (return . Left)

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
-- The operations are run outside of the catchIO cleanup handler because
-- catchIO masks asynchronous exceptions in the cleanup handler.
-- In the case of complete failure, the last exception is actually thrown.
firstSuccessful :: String -> [IO a] -> IO a
firstSuccessful caller = go Nothing
  where
  -- Attempt the next operation, remember exception on failure
  go _ (p:ps) =
    do r <- tryIO p
       case r of
         Right x -> return x
         Left  e -> go (Just e) ps

  -- All operations failed, throw error if one exists
  go Nothing  [] = ioError $ userError $ caller ++ ": firstSuccessful: empty list"
  go (Just e) [] = Exception.throwIO e
