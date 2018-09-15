{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Network.Socket.ByteString.Internal
-- Copyright   : (c) Johan Tibell 2007-2010
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : stable
-- Portability : portable
--
module Network.Socket.ByteString.Internal
    (
      mkInvalidRecvArgError
#if !defined(mingw32_HOST_OS)
    , c_writev
    , c_sendmsg
#endif
    , waitWhen0
    ) where

import System.IO.Error (ioeSetErrorString, mkIOError)

#if !defined(mingw32_HOST_OS)
import Foreign.C.Types (CInt(..))
import System.Posix.Types (CSsize(..))
import Foreign.Ptr (Ptr)

import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.ByteString.MsgHdr (MsgHdr)
#endif

import Control.Concurrent (threadWaitWrite, rtsSupportsBoundThreads)
import Control.Monad (when)
import GHC.IO.Exception (IOErrorType(..))
import Network.Socket.Types

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
#endif

waitWhen0 :: Int -> Socket -> IO ()
waitWhen0 0 s = when rtsSupportsBoundThreads $ do
  let fd = fromIntegral $ fdSocket s
  threadWaitWrite fd
waitWhen0 _ _ = return ()
