{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.Socket.ByteString.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- This module provides access to the BSD /socket/ interface.  This
-- module is generally more efficient than the 'String' based network
-- functions in 'Network.Socket'.  For detailed documentation, consult
-- your favorite POSIX socket reference. All functions communicate
-- failures by converting the error number to 'System.IO.IOError'.
--
-- This module is made to be imported with 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
--
module Network.Socket.ByteString.Lazy
    (
    -- * Send data to a socket
      send
    , sendAll
    ,

    -- * Receive data from a socket
      getContents
    , recv
    ) where

import Control.Monad (liftM)
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.Int (Int64)
import Network.Socket (Socket(..), ShutdownCmd(..), shutdown)
import Prelude hiding (getContents)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString as S
import qualified Network.Socket.ByteString as N

#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy.Windows (send, sendAll)
#else
import Network.Socket.ByteString.Lazy.Posix (send, sendAll)
#endif

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  Data is received on demand, in chunks; each chunk will be
-- sized to reflect the amount of data received by individual 'recv'
-- calls.
--
-- All remaining data from the socket is consumed.  When there is no
-- more data to be received, the receiving side of the socket is shut
-- down.  If there is an error and an exception is thrown, the socket
-- is not shut down.
getContents :: Socket         -- ^ Connected socket
            -> IO ByteString  -- ^ Data received
getContents sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock defaultChunkSize
    if S.null s
      then shutdown sock ShutdownReceive >> return Empty
      else Chunk s `liftM` loop

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the received data is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- If there is no more data to be received, returns an empty 'ByteString'.
--
-- Receiving data from closed socket may lead to undefined behaviour.
recv :: Socket         -- ^ Connected socket
     -> Int64          -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv sock nbytes = chunk `liftM` N.recv sock (fromIntegral nbytes) where
  chunk k
    | S.null k  = Empty
    | otherwise = Chunk k Empty
