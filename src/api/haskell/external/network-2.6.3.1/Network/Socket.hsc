{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- A higher level interface to networking operations is provided
-- through the module "Network".
--
-----------------------------------------------------------------------------

#include "HsNet.h"

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Types
      Socket(..)
    , Family(..)
    , isSupportedFamily
    , SocketType(..)
    , isSupportedSocketType
    , SockAddr(..)
    , isSupportedSockAddr
    , SocketStatus(..)
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
#if defined(IPV6_SOCKET_SUPPORT)
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
#endif
    , htonl
    , ntohl
    , ShutdownCmd(..)
    , ProtocolNumber
    , defaultProtocol
    , PortNumber(..)
    -- PortNumber is used non-abstractly in Network.BSD.  ToDo: remove
    -- this use and make the type abstract.

    -- * Address operations

    , HostName
    , ServiceName

#if defined(IPV6_SOCKET_SUPPORT)
    , AddrInfo(..)

    , AddrInfoFlag(..)
    , addrInfoFlagImplemented

    , defaultHints

    , getAddrInfo

    , NameInfoFlag(..)

    , getNameInfo
#endif

    -- * Socket operations
    , socket
#if defined(DOMAIN_SOCKET_SUPPORT)
    , socketPair
#endif
    , connect
    , bind
    , listen
    , accept
    , getPeerName
    , getSocketName

#if defined(HAVE_STRUCT_UCRED) || defined(HAVE_GETPEEREID)
    -- get the credentials of our domain socket peer.
    , getPeerCred
#if defined(HAVE_GETPEEREID)
    , getPeerEid
#endif
#endif

    , socketPort

    , socketToHandle

    -- ** Sending and receiving data
    -- *** Sending and receiving with String
    -- $sendrecv
    , send
    , sendTo
    , recv
    , recvFrom
    , recvLen

    -- *** Sending and receiving with a buffer
    , sendBuf
    , recvBuf
    , sendBufTo
    , recvBufFrom

    -- ** Misc
    , inet_addr
    , inet_ntoa

    , shutdown
    , close

    -- ** Predicates on sockets
    , isConnected
    , isBound
    , isListening
    , isReadable
    , isWritable

    -- * Socket options
    , SocketOption(..)
    , isSupportedSocketOption
    , getSocketOption
    , setSocketOption

    -- * File descriptor transmission
#ifdef DOMAIN_SOCKET_SUPPORT
    , sendFd
    , recvFd

#endif

    -- * Special constants
    , aNY_PORT
    , iNADDR_ANY
#if defined(IPV6_SOCKET_SUPPORT)
    , iN6ADDR_ANY
#endif
    , sOMAXCONN
    , sOL_SOCKET
#ifdef SCM_RIGHTS
    , sCM_RIGHTS
#endif
    , maxListenQueue

    -- * Initialisation
    , withSocketsDo

    -- * Very low level operations
    -- in case you ever want to get at the underlying file descriptor..
    , fdSocket
    , mkSocket

    -- * Deprecated aliases
    -- $deprecated-aliases
    , bindSocket
    , sClose
    , sIsConnected
    , sIsBound
    , sIsListening
    , sIsReadable
    , sIsWritable

    -- * Internal

    -- | The following are exported ONLY for use in the BSD module and
    -- should not be used anywhere else.

    , packFamily
    , unpackFamily
    , packSocketType
    ) where

import Data.Bits
import Data.Functor
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.Error
import Foreign.C.String (CString, withCString, withCStringLen, peekCString, peekCStringLen)
import Foreign.C.Types (CUInt, CChar)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Marshal.Utils ( maybeWith, with )

import System.IO
import Control.Monad (liftM, when)

import Control.Concurrent.MVar
import Data.Typeable
import System.IO.Error

import GHC.Conc (threadWaitRead, threadWaitWrite)
##if MIN_VERSION_base(4,3,1)
import GHC.Conc (closeFdWith)
##endif
# if defined(mingw32_HOST_OS)
import qualified Control.Exception as E
import GHC.Conc (asyncDoProc)
import GHC.IO.FD (FD(..), readRawBufferPtr, writeRawBufferPtr)
import Foreign (FunPtr)
# endif
# if defined(darwin_HOST_OS)
import Data.List (delete)
# endif
import qualified GHC.IO.Device
import GHC.IO.Handle.FD
import GHC.IO.Exception
import GHC.IO
import qualified System.Posix.Internals

import Network.Socket.Internal
import Network.Socket.Types

import Prelude -- Silence AMP warnings

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
type HostName       = String
type ServiceName    = String

-- ----------------------------------------------------------------------------
-- On Windows, our sockets are not put in non-blocking mode (non-blocking
-- is not supported for regular file descriptors on Windows, and it would
-- be a pain to support it only for sockets).  So there are two cases:
--
--  - the threaded RTS uses safe calls for socket operations to get
--    non-blocking I/O, just like the rest of the I/O library
--
--  - with the non-threaded RTS, only some operations on sockets will be
--    non-blocking.  Reads and writes go through the normal async I/O
--    system.  accept() uses asyncDoProc so is non-blocking.  A handful
--    of others (recvFrom, sendFd, recvFd) will block all threads - if this
--    is a problem, -threaded is the workaround.
--
##if defined(mingw32_HOST_OS)
##define SAFE_ON_WIN safe
##else
##define SAFE_ON_WIN unsafe
##endif

-----------------------------------------------------------------------------
-- Socket types

#if defined(mingw32_HOST_OS)
socket2FD  (MkSocket fd _ _ _ _) =
  -- HACK, 1 means True
  FD{fdFD = fd,fdIsSocket_ = 1}
#endif

mkSocket :: CInt
         -> Family
         -> SocketType
         -> ProtocolNumber
         -> SocketStatus
         -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   withSocketsDo $ return ()
   return (MkSocket fd fam sType pNum mStat)


fdSocket :: Socket -> CInt
fdSocket (MkSocket fd _ _ _ _) = fd

-- | This is the default protocol for a given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- SockAddr

instance Show SockAddr where
#if defined(DOMAIN_SOCKET_SUPPORT)
  showsPrec _ (SockAddrUnix str) = showString str
#endif
  showsPrec _ (SockAddrInet port ha)
   = showString (unsafePerformIO (inet_ntoa ha))
   . showString ":"
   . shows port
#if defined(IPV6_SOCKET_SUPPORT)
  showsPrec _ addr@(SockAddrInet6 port _ _ _)
   = showChar '['
   . showString (unsafePerformIO $
                 fst `liftM` getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString "]:"
   . shows port
#endif
#if defined(CAN_SOCKET_SUPPORT)
  showsPrec _ (SockAddrCan ifidx) = shows ifidx
#endif

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- | Create a new socket using the given address family, socket type
-- and protocol number.  The address family is usually 'AF_INET',
-- 'AF_INET6', or 'AF_UNIX'.  The socket type is usually 'Stream' or
-- 'Datagram'.  The protocol number is usually 'defaultProtocol'.
-- If 'AF_INET6' is used and the socket type is 'Stream' or 'Datagram',
-- the 'IPv6Only' socket option is set to 0 so that both IPv4 and IPv6
-- can be handled with one socket.
--
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
-- >>> sock@(MkSocket _ fam stype _ _) <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >>> fam
-- AF_INET
-- >>> stype
-- Stream
-- >>> bind sock (addrAddress addr)
-- >>> getSocketName sock
-- 127.0.0.1:5000
socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
socket family stype protocol = do
    c_stype <- packSocketTypeOrThrow "socket" stype
    fd <- throwSocketErrorIfMinus1Retry "socket" $
                c_socket (packFamily family) c_stype protocol
    setNonBlockIfNeeded fd
    socket_status <- newMVar NotConnected
    withSocketsDo $ return ()
    let sock = MkSocket fd family stype protocol socket_status
#if HAVE_DECL_IPV6_V6ONLY
    -- The default value of the IPv6Only option is platform specific,
    -- so we explicitly set it to 0 to provide a common default.
# if defined(mingw32_HOST_OS)
    -- The IPv6Only option is only supported on Windows Vista and later,
    -- so trying to change it might throw an error.
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      E.catch (setSocketOption sock IPv6Only 0) $ (\(_ :: E.IOException) -> return ())
# else
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      setSocketOption sock IPv6Only 0 `onException` close sock
# endif
#endif
    return sock

-- | Build a pair of connected socket objects using the given address
-- family, socket type, and protocol number.  Address family, socket
-- type, and protocol number are as for the 'socket' function above.
-- Availability: Unix.
#if defined(DOMAIN_SOCKET_SUPPORT)
socketPair :: Family              -- Family Name (usually AF_INET or AF_INET6)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
    c_stype <- packSocketTypeOrThrow "socketPair" stype
    _rc <- throwSocketErrorIfMinus1Retry "socketpair" $
                c_socketpair (packFamily family) c_stype protocol fdArr
    [fd1,fd2] <- peekArray 2 fdArr
    s1 <- mkNonBlockingSocket fd1
    s2 <- mkNonBlockingSocket fd2
    return (s1,s2)
  where
    mkNonBlockingSocket fd = do
       setNonBlockIfNeeded fd
       stat <- newMVar Connected
       withSocketsDo $ return ()
       return (MkSocket fd family stype protocol stat)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
#endif

-- | Set the socket to nonblocking, if applicable to this platform.
setNonBlockIfNeeded :: CInt -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD fd True

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'aNY_PORT' is passed then the system assigns the next available
-- use port.
bind :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> IO ()
bind (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= NotConnected
  then
   ioError (userError ("bind: can't peform bind on socket in status " ++
         show status))
  else do
   withSockAddr addr $ \p_addr sz -> do
   _status <- throwSocketErrorIfMinus1Retry "bind" $ c_bind s p_addr (fromIntegral sz)
   return Bound

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> IO ()
connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = withSocketsDo $ do
 modifyMVar_ socketStatus $ \currentStatus -> do
 if currentStatus /= NotConnected && currentStatus /= Bound
  then
    ioError (userError ("connect: can't peform connect on socket in status " ++
        show currentStatus))
  else do
    withSockAddr addr $ \p_addr sz -> do

    let connectLoop = do
           r <- c_connect s p_addr (fromIntegral sz)
           if r == -1
               then do
#if !(defined(HAVE_WINSOCK2_H))
                   err <- getErrno
                   case () of
                     _ | err == eINTR       -> connectLoop
                     _ | err == eINPROGRESS -> connectBlocked
--                   _ | err == eAGAIN      -> connectBlocked
                     _otherwise             -> throwSocketError "connect"
#else
                   throwSocketError "connect"
#endif
               else return ()

        connectBlocked = do
           threadWaitWrite (fromIntegral s)
           err <- getSocketOption sock SoError
           if (err == 0)
                then return ()
                else throwSocketErrorCode "connect" (fromIntegral err)

    connectLoop
    return Connected

-----------------------------------------------------------------------------
-- Listen

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket  -- Connected & Bound Socket
       -> Int     -- Queue Length
       -> IO ()
listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= Bound
   then
     ioError (userError ("listen: can't peform listen on socket in status " ++
         show status))
   else do
     throwSocketErrorIfMinus1Retry_ "listen" (c_listen s (fromIntegral backlog))
     return Listening

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

-- | Accept a connection.  The socket must be bound to an address and
-- listening for connections.  The return value is a pair @(conn,
-- address)@ where @conn@ is a new socket object usable to send and
-- receive data on the connection, and @address@ is the address bound
-- to the socket on the other end of the connection.
accept :: Socket                        -- Queue Socket
       -> IO (Socket,                   -- Readable Socket
              SockAddr)                 -- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- isAcceptable sock
 if not okay
   then
     ioError (userError ("accept: can't perform accept on socket (" ++ (show (family,stype,protocol)) ++") in status " ++
         show currentStatus))
   else do
     let sz = sizeOfSockAddrByFamily family
     allocaBytes sz $ \ sockaddr -> do
#if defined(mingw32_HOST_OS)
     new_sock <-
        if threaded
           then with (fromIntegral sz) $ \ ptr_len ->
                  throwSocketErrorIfMinus1Retry "Network.Socket.accept" $
                    c_accept_safe s sockaddr ptr_len
           else do
                paramData <- c_newAcceptParams s (fromIntegral sz) sockaddr
                rc        <- asyncDoProc c_acceptDoProc paramData
                new_sock  <- c_acceptNewSock    paramData
                c_free paramData
                when (rc /= 0) $
                     throwSocketErrorCode "Network.Socket.accept" (fromIntegral rc)
                return new_sock
#else
     with (fromIntegral sz) $ \ ptr_len -> do
# ifdef HAVE_ACCEPT4
     new_sock <- throwSocketErrorIfMinus1RetryMayBlock "accept"
                        (threadWaitRead (fromIntegral s))
                        (c_accept4 s sockaddr ptr_len (#const SOCK_NONBLOCK))
# else
     new_sock <- throwSocketErrorWaitRead sock "accept"
                        (c_accept s sockaddr ptr_len)
     setNonBlockIfNeeded new_sock
# endif /* HAVE_ACCEPT4 */
#endif
     addr <- peekSockAddr sockaddr
     new_status <- newMVar Connected
     return ((MkSocket new_sock family stype protocol new_status), addr)

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "HsNet.h acceptNewSock"
  c_acceptNewSock :: Ptr () -> IO CInt
foreign import ccall unsafe "HsNet.h newAcceptParams"
  c_newAcceptParams :: CInt -> CInt -> Ptr a -> IO (Ptr ())
foreign import ccall unsafe "HsNet.h &acceptDoProc"
  c_acceptDoProc :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "free"
  c_free:: Ptr a -> IO ()
#endif

-----------------------------------------------------------------------------
-- ** Sending and reciving data

-- $sendrecv
--
-- Do not use the @send@ and @recv@ functions defined in this section
-- in new code, as they incorrectly represent binary data as a Unicode
-- string.  As a result, these functions are inefficient and may lead
-- to bugs in the program.  Instead use the @send@ and @recv@
-- functions defined in the "Network.Socket.ByteString" module.

-----------------------------------------------------------------------------
-- sendTo & recvFrom

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
{-# WARNING sendTo "Use sendTo defined in \"Network.Socket.ByteString\"" #-}
sendTo :: Socket        -- (possibly) bound/connected Socket
       -> String        -- Data to send
       -> SockAddr
       -> IO Int        -- Number of Bytes sent
sendTo sock xs addr = do
 withCStringLen xs $ \(str, len) -> do
   sendBufTo sock str len addr

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufTo :: Socket            -- (possibly) bound/connected Socket
          -> Ptr a -> Int  -- Data to send
          -> SockAddr
          -> IO Int            -- Number of Bytes sent
sendBufTo sock@(MkSocket s _family _stype _protocol _status) ptr nbytes addr = do
 withSockAddr addr $ \p_addr sz -> do
   liftM fromIntegral $
     throwSocketErrorWaitWrite sock "sendTo" $
        c_sendto s ptr (fromIntegral $ nbytes) 0{-flags-}
                        p_addr (fromIntegral sz)

-- | Receive data from the socket. The socket need not be in a
-- connected state. Returns @(bytes, nbytes, address)@ where @bytes@
-- is a @String@ of length @nbytes@ representing the data received and
-- @address@ is a 'SockAddr' representing the address of the sending
-- socket.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
{-# WARNING recvFrom "Use recvFrom defined in \"Network.Socket.ByteString\"" #-}
recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock nbytes =
  allocaBytes nbytes $ \ptr -> do
    (len, sockaddr) <- recvBufFrom sock ptr nbytes
    str <- peekCStringLen (ptr, len)
    return (str, len, sockaddr)

-- | Receive data from the socket, writing it into buffer instead of
-- creating a new string.  The socket need not be in a connected
-- state. Returns @(nbytes, address)@ where @nbytes@ is the number of
-- bytes received and @address@ is a 'SockAddr' representing the
-- address of the sending socket.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom sock@(MkSocket s family _stype _protocol _status) ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   =
    withNewSockAddr family $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
        poke ptr_len (fromIntegral sz)
        len <- throwSocketErrorWaitRead sock "recvFrom" $
                   c_recvfrom s ptr (fromIntegral nbytes) 0{-flags-}
                                ptr_addr ptr_len
        let len' = fromIntegral len
        if len' == 0
         then ioError (mkEOFError "Network.Socket.recvFrom")
         else do
           flg <- isConnected sock
             -- For at least one implementation (WinSock 2), recvfrom() ignores
             -- filling in the sockaddr for connected TCP sockets. Cope with
             -- this by using getPeerName instead.
           sockaddr <-
                if flg then
                   getPeerName sock
                else
                   peekSockAddr ptr_addr
           return (len', sockaddr)

-----------------------------------------------------------------------------
-- send & recv

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
{-# WARNING send "Use send defined in \"Network.Socket.ByteString\"" #-}
send :: Socket  -- Bound/Connected Socket
     -> String  -- Data to send
     -> IO Int  -- Number of Bytes sent
send sock xs = withCStringLen xs $ \(str, len) ->
    sendBuf sock (castPtr str) len

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendBuf :: Socket     -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> IO Int     -- Number of Bytes sent
sendBuf sock@(MkSocket s _family _stype _protocol _status) str len = do
   liftM fromIntegral $
#if defined(mingw32_HOST_OS)
-- writeRawBufferPtr is supposed to handle checking for errors, but it's broken
-- on x86_64 because of GHC bug #12010 so we duplicate the check here. The call
-- to throwSocketErrorIfMinus1Retry can be removed when no GHC version with the
-- bug is supported.
    throwSocketErrorIfMinus1Retry "Network.Socket.sendBuf" $ writeRawBufferPtr
      "Network.Socket.sendBuf"
      (socket2FD sock)
      (castPtr str)
      0
      (fromIntegral len)
#else
     throwSocketErrorWaitWrite sock "sendBuf" $
        c_send s str (fromIntegral len) 0{-flags-}
#endif


-- | Receive data from the socket.  The socket must be in a connected
-- state. This function may return fewer bytes than specified.  If the
-- message is longer than the specified length, it may be discarded
-- depending on the type of socket.  This function may block until a
-- message arrives.
--
-- Considering hardware and network realities, the maximum number of
-- bytes to receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
--
-- Receiving data from closed socket may lead to undefined behaviour.
{-# WARNING recv "Use recv defined in \"Network.Socket.ByteString\"" #-}
recv :: Socket -> Int -> IO String
recv sock l = fst <$> recvLen sock l

{-# WARNING recvLen "Use recvLen defined in \"Network.Socket.ByteString\"" #-}
recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock nbytes =
     allocaBytes nbytes $ \ptr -> do
        len <- recvBuf sock ptr nbytes
        s <- peekCStringLen (castPtr ptr,len)
        return (s, len)

-- | Receive data from the socket.  The socket must be in a connected
-- state. This function may return fewer bytes than specified.  If the
-- message is longer than the specified length, it may be discarded
-- depending on the type of socket.  This function may block until a
-- message arrives.
--
-- Considering hardware and network realities, the maximum number of
-- bytes to receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
--
-- Receiving data from closed socket may lead to undefined behaviour.
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf sock@(MkSocket s _family _stype _protocol _status) ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBuf")
 | otherwise   = do
        len <-
#if defined(mingw32_HOST_OS)
-- see comment in sendBuf above.
            throwSocketErrorIfMinus1Retry "Network.Socket.recvBuf" $
                readRawBufferPtr "Network.Socket.recvBuf"
                (socket2FD sock) ptr 0 (fromIntegral nbytes)
#else
               throwSocketErrorWaitRead sock "recvBuf" $
                   c_recv s (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
#endif
        let len' = fromIntegral len
        if len' == 0
         then ioError (mkEOFError "Network.Socket.recvBuf")
         else return len'


-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port
#if defined(IPV6_SOCKET_SUPPORT)
socketPort sock@(MkSocket _ AF_INET6 _ _ _) = do
    (SockAddrInet6 port _ _ _) <- getSocketName sock
    return port
#endif
socketPort (MkSocket _ family _ _ _) =
    ioError (userError ("socketPort: not supported for Family " ++ show family))


-- ---------------------------------------------------------------------------
-- getPeerName

-- Calling $getPeerName$ returns the address details of the machine,
-- other than the local one, which is connected to the socket. This is
-- used in programs such as FTP to determine where to send the
-- returning data.  The corresponding call to get the details of the
-- local machine is $getSocketName$.

getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry_ "getPeerName" $ c_getpeername s ptr int_star
   _sz <- peek int_star
   peekSockAddr ptr

getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry_ "getSocketName" $ c_getsockname s ptr int_star
   peekSockAddr ptr

-----------------------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption
    = Debug         -- ^ SO_DEBUG
    | ReuseAddr     -- ^ SO_REUSEADDR
    | Type          -- ^ SO_TYPE
    | SoError       -- ^ SO_ERROR
    | DontRoute     -- ^ SO_DONTROUTE
    | Broadcast     -- ^ SO_BROADCAST
    | SendBuffer    -- ^ SO_SNDBUF
    | RecvBuffer    -- ^ SO_RCVBUF
    | KeepAlive     -- ^ SO_KEEPALIVE
    | OOBInline     -- ^ SO_OOBINLINE
    | TimeToLive    -- ^ IP_TTL
    | MaxSegment    -- ^ TCP_MAXSEG
    | NoDelay       -- ^ TCP_NODELAY
    | Cork          -- ^ TCP_CORK
    | Linger        -- ^ SO_LINGER
    | ReusePort     -- ^ SO_REUSEPORT
    | RecvLowWater  -- ^ SO_RCVLOWAT
    | SendLowWater  -- ^ SO_SNDLOWAT
    | RecvTimeOut   -- ^ SO_RCVTIMEO
    | SendTimeOut   -- ^ SO_SNDTIMEO
    | UseLoopBack   -- ^ SO_USELOOPBACK
    | UserTimeout   -- ^ TCP_USER_TIMEOUT
    | IPv6Only      -- ^ IPV6_V6ONLY
    | CustomSockOpt (CInt, CInt)
    deriving (Show, Typeable)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption = isJust . packSocketOption

-- | For a socket option, return Just (level, value) where level is the
-- corresponding C option level constant (e.g. SOL_SOCKET) and value is
-- the option constant itself (e.g. SO_DEBUG)
-- If either constant does not exist, return Nothing.
packSocketOption :: SocketOption -> Maybe (CInt, CInt)
packSocketOption so =
  -- The Just here is a hack to disable GHC's overlapping pattern detection:
  -- the problem is if all constants are present, the fallback pattern is
  -- redundant, but if they aren't then it isn't. Hence we introduce an
  -- extra pattern (Nothing) that can't possibly happen, so that the
  -- fallback is always (in principle) necessary.
  -- I feel a little bad for including this, but such are the sacrifices we
  -- make while working with CPP - excluding the fallback pattern correctly
  -- would be a serious nuisance.
  -- (NB: comments elsewhere in this file refer to this one)
  case Just so of
#ifdef SOL_SOCKET
#ifdef SO_DEBUG
    Just Debug         -> Just ((#const SOL_SOCKET), (#const SO_DEBUG))
#endif
#ifdef SO_REUSEADDR
    Just ReuseAddr     -> Just ((#const SOL_SOCKET), (#const SO_REUSEADDR))
#endif
#ifdef SO_TYPE
    Just Type          -> Just ((#const SOL_SOCKET), (#const SO_TYPE))
#endif
#ifdef SO_ERROR
    Just SoError       -> Just ((#const SOL_SOCKET), (#const SO_ERROR))
#endif
#ifdef SO_DONTROUTE
    Just DontRoute     -> Just ((#const SOL_SOCKET), (#const SO_DONTROUTE))
#endif
#ifdef SO_BROADCAST
    Just Broadcast     -> Just ((#const SOL_SOCKET), (#const SO_BROADCAST))
#endif
#ifdef SO_SNDBUF
    Just SendBuffer    -> Just ((#const SOL_SOCKET), (#const SO_SNDBUF))
#endif
#ifdef SO_RCVBUF
    Just RecvBuffer    -> Just ((#const SOL_SOCKET), (#const SO_RCVBUF))
#endif
#ifdef SO_KEEPALIVE
    Just KeepAlive     -> Just ((#const SOL_SOCKET), (#const SO_KEEPALIVE))
#endif
#ifdef SO_OOBINLINE
    Just OOBInline     -> Just ((#const SOL_SOCKET), (#const SO_OOBINLINE))
#endif
#ifdef SO_LINGER
    Just Linger        -> Just ((#const SOL_SOCKET), (#const SO_LINGER))
#endif
#ifdef SO_REUSEPORT
    Just ReusePort     -> Just ((#const SOL_SOCKET), (#const SO_REUSEPORT))
#endif
#ifdef SO_RCVLOWAT
    Just RecvLowWater  -> Just ((#const SOL_SOCKET), (#const SO_RCVLOWAT))
#endif
#ifdef SO_SNDLOWAT
    Just SendLowWater  -> Just ((#const SOL_SOCKET), (#const SO_SNDLOWAT))
#endif
#ifdef SO_RCVTIMEO
    Just RecvTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_RCVTIMEO))
#endif
#ifdef SO_SNDTIMEO
    Just SendTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_SNDTIMEO))
#endif
#ifdef SO_USELOOPBACK
    Just UseLoopBack   -> Just ((#const SOL_SOCKET), (#const SO_USELOOPBACK))
#endif
#endif // SOL_SOCKET
#if HAVE_DECL_IPPROTO_IP
#ifdef IP_TTL
    Just TimeToLive    -> Just ((#const IPPROTO_IP), (#const IP_TTL))
#endif
#endif // HAVE_DECL_IPPROTO_IP
#if HAVE_DECL_IPPROTO_TCP
#ifdef TCP_MAXSEG
    Just MaxSegment    -> Just ((#const IPPROTO_TCP), (#const TCP_MAXSEG))
#endif
#ifdef TCP_NODELAY
    Just NoDelay       -> Just ((#const IPPROTO_TCP), (#const TCP_NODELAY))
#endif
#ifdef TCP_USER_TIMEOUT
    Just UserTimeout   -> Just ((#const IPPROTO_TCP), (#const TCP_USER_TIMEOUT))
#endif
#ifdef TCP_CORK
    Just Cork          -> Just ((#const IPPROTO_TCP), (#const TCP_CORK))
#endif
#endif // HAVE_DECL_IPPROTO_TCP
#if HAVE_DECL_IPPROTO_IPV6
#if HAVE_DECL_IPV6_V6ONLY
    Just IPv6Only      -> Just ((#const IPPROTO_IPV6), (#const IPV6_V6ONLY))
#endif
#endif // HAVE_DECL_IPPROTO_IPV6
    Just (CustomSockOpt opt) -> Just opt
    _             -> Nothing

-- | Return the option level and option value if they exist,
-- otherwise throw an error that begins "Network.Socket." ++ the String
-- parameter
packSocketOption' :: String -> SocketOption -> IO (CInt, CInt)
packSocketOption' caller so = maybe err return (packSocketOption so)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller,
    ": socket option ", show so, " unsupported on this system"]

-- | Set a socket option that expects an Int value.
-- There is currently no API to set e.g. the timeval socket options
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   (level, opt) <- packSocketOption' "setSocketOption" so
   with (fromIntegral v) $ \ptr_v -> do
   throwSocketErrorIfMinus1_ "setSocketOption" $
       c_setsockopt s level opt ptr_v
          (fromIntegral (sizeOf (undefined :: CInt)))
   return ()


-- | Get a socket option that gives an Int value.
-- There is currently no API to get e.g. the timeval socket options
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   (level, opt) <- packSocketOption' "getSocketOption" so
   alloca $ \ptr_v ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \ptr_sz -> do
       throwSocketErrorIfMinus1Retry_ "getSocketOption" $
         c_getsockopt s level opt ptr_v ptr_sz
       fromIntegral `liftM` peek ptr_v


#if defined(HAVE_STRUCT_UCRED) || defined(HAVE_GETPEEREID)
-- | Returns the processID, userID and groupID of the socket's peer.
--
-- Only available on platforms that support SO_PEERCRED or GETPEEREID(3)
-- on domain sockets.
-- GETPEEREID(3) returns userID and groupID. processID is always 0.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred sock = do
#ifdef HAVE_STRUCT_UCRED
  let fd = fdSocket sock
  let sz = (#const sizeof(struct ucred))
  allocaBytes sz $ \ ptr_cr ->
   with (fromIntegral sz) $ \ ptr_sz -> do
     _ <- ($) throwSocketErrorIfMinus1Retry "getPeerCred" $
       c_getsockopt fd (#const SOL_SOCKET) (#const SO_PEERCRED) ptr_cr ptr_sz
     pid <- (#peek struct ucred, pid) ptr_cr
     uid <- (#peek struct ucred, uid) ptr_cr
     gid <- (#peek struct ucred, gid) ptr_cr
     return (pid, uid, gid)
#else
  (uid,gid) <- getPeerEid sock
  return (0,uid,gid)
#endif

#ifdef HAVE_GETPEEREID
-- | The getpeereid() function returns the effective user and group IDs of the
-- peer connected to a UNIX-domain socket
getPeerEid :: Socket -> IO (CUInt, CUInt)
getPeerEid sock = do
  let fd = fdSocket sock
  alloca $ \ ptr_uid ->
    alloca $ \ ptr_gid -> do
      throwSocketErrorIfMinus1Retry_ "getPeerEid" $
        c_getpeereid fd ptr_uid ptr_gid
      uid <- peek ptr_uid
      gid <- peek ptr_gid
      return (uid, gid)
#endif
#endif

##if !(MIN_VERSION_base(4,3,1))
closeFdWith closer fd = closer fd
##endif

#if defined(DOMAIN_SOCKET_SUPPORT)
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  _ <- ($) throwSocketErrorWaitWrite sock "sendFd" $
     c_sendFd (fdSocket sock) outfd
   -- Note: If Winsock supported FD-passing, thi would have been
   -- incorrect (since socket FDs need to be closed via closesocket().)
  closeFd outfd

recvFd :: Socket -> IO CInt
recvFd sock = do
  theFd <- throwSocketErrorWaitRead sock "recvFd" $
               c_recvFd (fdSocket sock)
  return theFd

foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt

#endif

-- ---------------------------------------------------------------------------
-- Utility Functions

aNY_PORT :: PortNumber
aNY_PORT = 0

-- | The IPv4 wild card address.

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl (#const INADDR_ANY)

-- | Converts the from host byte order to network byte order.
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
-- | Converts the from network byte order to host byte order.
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32

#if defined(IPV6_SOCKET_SUPPORT)
-- | The IPv6 wild card address.

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY = (0, 0, 0, 0)
#endif

sOMAXCONN :: Int
sOMAXCONN = #const SOMAXCONN

sOL_SOCKET :: Int
sOL_SOCKET = #const SOL_SOCKET

#ifdef SCM_RIGHTS
sCM_RIGHTS :: Int
sCM_RIGHTS = #const SCM_RIGHTS
#endif

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = sOMAXCONN

-- -----------------------------------------------------------------------------

data ShutdownCmd
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth
 deriving Typeable

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

-- | Shut down one or both halves of the connection, depending on the
-- second argument to the function.  If the second argument is
-- 'ShutdownReceive', further receives are disallowed.  If it is
-- 'ShutdownSend', further sends are disallowed.  If it is
-- 'ShutdownBoth', further sends and receives are disallowed.
shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry_ "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

-- | Close the socket. Sending data to or receiving data from closed socket
-- may lead to undefined behaviour.
close :: Socket -> IO ()
close (MkSocket s _ _ _ socketStatus) = do
 modifyMVar_ socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
         ioError (userError ("close: converted to a Handle, use hClose instead"))
     Closed ->
         return status
     _ -> closeFdWith (closeFd . fromIntegral) (fromIntegral s) >> return Closed

-- -----------------------------------------------------------------------------

-- | Determines whether 'close' has been used on the 'Socket'. This
-- does /not/ indicate any status about the socket beyond this. If the
-- socket has been closed remotely, this function can still return
-- 'True'.
isConnected :: Socket -> IO Bool
isConnected (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected)

-- -----------------------------------------------------------------------------
-- Socket Predicates

isBound :: Socket -> IO Bool
isBound (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Bound)

isListening :: Socket -> IO Bool
isListening (MkSocket _ _ _  _ status) = do
    value <- readMVar status
    return (value == Listening)

isReadable  :: Socket -> IO Bool
isReadable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Listening || value == Connected)

isWritable  :: Socket -> IO Bool
isWritable = isReadable -- sort of.

isAcceptable :: Socket -> IO Bool
#if defined(DOMAIN_SOCKET_SUPPORT)
isAcceptable (MkSocket _ AF_UNIX x _ status)
    | x == Stream || x == SeqPacket = do
        value <- readMVar status
        return (value == Connected || value == Bound || value == Listening)
isAcceptable (MkSocket _ AF_UNIX _ _ _) = return False
#endif
isAcceptable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected || value == Listening)

-- -----------------------------------------------------------------------------
-- Internet address manipulation routines:

inet_addr :: String -> IO HostAddress
inet_addr ipstr = withSocketsDo $ do
   withCString ipstr $ \str -> do
   had <- c_inet_addr str
   if had == -1
    then ioError (userError ("inet_addr: Malformed address: " ++ ipstr))
    else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = withSocketsDo $ do
  pstr <- c_inet_ntoa haddr
  peekCString pstr

-- | Turns a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'socketToHandle'.  To
-- close the 'Socket' after 'socketToHandle', call 'System.IO.hClose'
-- on the 'Handle'.

socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ socketStatus) mode = do
 modifyMVar socketStatus $ \ status ->
    if status == ConvertedToHandle
        then ioError (userError ("socketToHandle: already a Handle"))
        else do
    h <- fdToHandle' (fromIntegral fd) (Just GHC.IO.Device.Stream) True (show s) mode True{-bin-}
    hSetBuffering h NoBuffering
    return (ConvertedToHandle, h)

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b

packBits mapping xs = foldl' pack 0 mapping
    where pack acc (k, v) | k `elem` xs = acc .|. v
                          | otherwise   = acc

-- | Unpack a bitmask into a list of values.

unpackBits :: (Num b, Bits b) => [(a, b)] -> b -> [a]

-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- Address and service lookups

#if defined(IPV6_SOCKET_SUPPORT)

-- | Flags that control the querying behaviour of 'getAddrInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-25>
data AddrInfoFlag =
    -- | The list of returned 'AddrInfo' values will
    --   only contain IPv4 addresses if the local system has at least
    --   one IPv4 interface configured, and likewise for IPv6.
    --   (Only some platforms support this.)
      AI_ADDRCONFIG
    -- | If 'AI_ALL' is specified, return all matching IPv6 and
    --   IPv4 addresses.  Otherwise, this flag has no effect.
    --   (Only some platforms support this.)
    | AI_ALL
    -- | The 'addrCanonName' field of the first returned
    --   'AddrInfo' will contain the "canonical name" of the host.
    | AI_CANONNAME
    -- | The 'HostName' argument /must/ be a numeric
    --   address in string form, and network name lookups will not be
    --   attempted.
    | AI_NUMERICHOST
    -- | The 'ServiceName' argument /must/ be a port
    --   number in string form, and service name lookups will not be
    --   attempted. (Only some platforms support this.)
    | AI_NUMERICSERV
    -- | If no 'HostName' value is provided, the network
    --   address in each 'SockAddr'
    --   will be left as a "wild card", i.e. as either 'iNADDR_ANY'
    --   or 'iN6ADDR_ANY'.  This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show, Typeable)

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [
#if HAVE_DECL_AI_ADDRCONFIG
     (AI_ADDRCONFIG, #const AI_ADDRCONFIG),
#else
     (AI_ADDRCONFIG, 0),
#endif
#if HAVE_DECL_AI_ALL
     (AI_ALL, #const AI_ALL),
#else
     (AI_ALL, 0),
#endif
     (AI_CANONNAME, #const AI_CANONNAME),
     (AI_NUMERICHOST, #const AI_NUMERICHOST),
#if HAVE_DECL_AI_NUMERICSERV
     (AI_NUMERICSERV, #const AI_NUMERICSERV),
#else
     (AI_NUMERICSERV, 0),
#endif
     (AI_PASSIVE, #const AI_PASSIVE),
#if HAVE_DECL_AI_V4MAPPED
     (AI_V4MAPPED, #const AI_V4MAPPED)
#else
     (AI_V4MAPPED, 0)
#endif
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo =
    AddrInfo {
        addrFlags :: [AddrInfoFlag],
        addrFamily :: Family,
        addrSocketType :: SocketType,
        addrProtocol :: ProtocolNumber,
        addrAddress :: SockAddr,
        addrCanonName :: Maybe String
        }
    deriving (Eq, Show, Typeable)

instance Storable AddrInfo where
    sizeOf    _ = #const sizeof(struct addrinfo)
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        ai_flags <- (#peek struct addrinfo, ai_flags) p
        ai_family <- (#peek struct addrinfo, ai_family) p
        ai_socktype <- (#peek struct addrinfo, ai_socktype) p
        ai_protocol <- (#peek struct addrinfo, ai_protocol) p
        ai_addr <- (#peek struct addrinfo, ai_addr) p >>= peekSockAddr
        ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else liftM Just $ peekCString ai_canonname_ptr

        socktype <- unpackSocketType' "AddrInfo.peek" ai_socktype
        return (AddrInfo
                {
                 addrFlags = unpackBits aiFlagMapping ai_flags,
                 addrFamily = unpackFamily ai_family,
                 addrSocketType = socktype,
                 addrProtocol = ai_protocol,
                 addrAddress = ai_addr,
                 addrCanonName = ai_canonname
                })

    poke p (AddrInfo flags family socketType protocol _ _) = do
        c_stype <- packSocketTypeOrThrow "AddrInfo.poke" socketType

        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p (packFamily family)
        (#poke struct addrinfo, ai_socktype) p c_stype
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

-- | Flags that control the querying behaviour of 'getNameInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-30>
data NameInfoFlag =
    -- | Resolve a datagram-based service name.  This is
    --   required only for the few protocols that have different port
    --   numbers for their datagram-based versions than for their
    --   stream-based versions.
      NI_DGRAM
    -- | If the hostname cannot be looked up, an IO error is thrown.
    | NI_NAMEREQD
    -- | If a host is local, return only the hostname part of the FQDN.
    | NI_NOFQDN
    -- | The name of the host is not looked up.
    --   Instead, a numeric representation of the host's
    --   address is returned.  For an IPv4 address, this will be a
    --   dotted-quad string.  For IPv6, it will be colon-separated
    --   hexadecimal.
    | NI_NUMERICHOST
    -- | The name of the service is not
    --   looked up.  Instead, a numeric representation of the
    --   service is returned.
    | NI_NUMERICSERV
    deriving (Eq, Read, Show, Typeable)

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, #const NI_DGRAM),
                 (NI_NAMEREQD, #const NI_NAMEREQD),
                 (NI_NOFQDN, #const NI_NOFQDN),
                 (NI_NUMERICHOST, #const NI_NUMERICHOST),
                 (NI_NUMERICSERV, #const NI_NUMERICSERV)]

-- | Default hints for address lookup with 'getAddrInfo'.  The values
-- of the 'addrAddress' and 'addrCanonName' fields are 'undefined',
-- and are never inspected by 'getAddrInfo'.
--
-- >>> addrFlags defaultHints
-- []
-- >>> addrFamily defaultHints
-- AF_UNSPEC
-- >>> addrSocketType defaultHints
-- NoSocketType
-- >>> addrProtocol defaultHints
-- 0

defaultHints :: AddrInfo
defaultHints = AddrInfo {
                         addrFlags = [],
                         addrFamily = AF_UNSPEC,
                         addrSocketType = NoSocketType,
                         addrProtocol = defaultProtocol,
                         addrAddress = undefined,
                         addrCanonName = undefined
                        }

-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bind'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
--
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
-- >>> addrAddress addr
-- 127.0.0.1:80

getAddrInfo :: Maybe AddrInfo -- ^ preferred socket type or protocol
            -> Maybe HostName -- ^ host name to look up
            -> Maybe ServiceName -- ^ service name to look up
            -> IO [AddrInfo] -- ^ resolved addresses, with "best" first

getAddrInfo hints node service = withSocketsDo $
  maybeWith withCString node $ \c_node ->
    maybeWith withCString service $ \c_service ->
      maybeWith with filteredHints $ \c_hints ->
        alloca $ \ptr_ptr_addrs -> do
          ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
          case ret of
            0 -> do ptr_addrs <- peek ptr_ptr_addrs
                    ais <- followAddrInfo ptr_addrs
                    c_freeaddrinfo ptr_addrs
                    return ais
            _ -> do err <- gai_strerror ret
                    ioError (ioeSetErrorString
                             (mkIOError NoSuchThing "getAddrInfo" Nothing
                              Nothing) err)
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
  where
#if defined(darwin_HOST_OS)
    filteredHints = case service of
        Nothing -> fmap (\ h -> h { addrFlags = delete AI_NUMERICSERV (addrFlags h) }) hints
        _       -> hints
#else
    filteredHints = hints
#endif

followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]

followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                      | otherwise = do
    a <- peek ptr_ai
    as <- (#peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
    return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String

#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString
#else
gai_strerror n = return ("error " ++ show n)
#endif

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True n f = allocaBytes n (f (fromIntegral n))

-- | Resolve an address to a host or service name.
-- This function is protocol independent.
-- The list of 'NameInfoFlag' values controls query behaviour.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- Example:
-- @
--   (hostName, _) <- getNameInfo [] True False myAddress
-- @

getNameInfo :: [NameInfoFlag] -- ^ flags to control lookup behaviour
            -> Bool -- ^ whether to look up a hostname
            -> Bool -- ^ whether to look up a service name
            -> SockAddr -- ^ the address to look up
            -> IO (Maybe HostName, Maybe ServiceName)

getNameInfo flags doHost doService addr = withSocketsDo $
  withCStringIf doHost (#const NI_MAXHOST) $ \c_hostlen c_host ->
    withCStringIf doService (#const NI_MAXSERV) $ \c_servlen c_serv -> do
      withSockAddr addr $ \ptr_addr sz -> do
        ret <- c_getnameinfo ptr_addr (fromIntegral sz) c_host c_hostlen
                             c_serv c_servlen (packBits niFlagMapping flags)
        case ret of
          0 -> do
            let peekIf doIf c_val = if doIf
                                     then liftM Just $ peekCString c_val
                                     else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          _ -> do err <- gai_strerror ret
                  ioError (ioeSetErrorString
                           (mkIOError NoSuchThing "getNameInfo" Nothing
                            Nothing) err)

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

mkEOFError :: String -> IOError
mkEOFError loc = ioeSetErrorString (mkIOError EOF loc Nothing Nothing) "end of file"

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "my_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import CALLCONV unsafe "inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

closeFd :: CInt -> IO ()
closeFd fd = throwSocketErrorIfMinus1_ "Network.Socket.close" $ c_close fd

#if !defined(WITH_WINSOCK)
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#else
foreign import stdcall unsafe "closesocket"
  c_close :: CInt -> IO CInt
#endif

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
#ifdef HAVE_ACCEPT4
foreign import CALLCONV unsafe "accept4"
  c_accept4 :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> CInt -> IO CInt
#else
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
#endif
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

#if defined(mingw32_HOST_OS)
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
#endif

foreign import CALLCONV unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "sendto"
  c_sendto :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import CALLCONV unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "recvfrom"
  c_recvfrom :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

#if defined(HAVE_GETPEEREID)
foreign import CALLCONV unsafe "getpeereid"
  c_getpeereid :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
#endif
-- ---------------------------------------------------------------------------
-- * Deprecated aliases

-- $deprecated-aliases
--
-- These aliases are deprecated and should not be used in new code.
-- They will be removed in some future version of the package.

{-# DEPRECATED bindSocket "use 'bind'" #-}

-- | Deprecated alias for 'bind'.
bindSocket :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> IO ()
bindSocket = bind

{-# DEPRECATED sClose "use 'close'" #-}

-- | Deprecated alias for 'close'.
sClose :: Socket -> IO ()
sClose = close

{-# DEPRECATED sIsConnected "use 'isConnected'" #-}

-- | Deprecated alias for 'isConnected'.
sIsConnected :: Socket -> IO Bool
sIsConnected = isConnected

{-# DEPRECATED sIsBound "use 'isBound'" #-}

-- | Deprecated alias for 'isBound'.
sIsBound :: Socket -> IO Bool
sIsBound = isBound

{-# DEPRECATED sIsListening "use 'isListening'" #-}

-- | Deprecated alias for 'isListening'.
sIsListening :: Socket -> IO Bool
sIsListening = isListening

{-# DEPRECATED sIsReadable "use 'isReadable'" #-}

-- | Deprecated alias for 'isReadable'.
sIsReadable  :: Socket -> IO Bool
sIsReadable = isReadable

{-# DEPRECATED sIsWritable "use 'isWritable'" #-}

-- | Deprecated alias for 'isWritable'.
sIsWritable  :: Socket -> IO Bool
sIsWritable = isWritable
