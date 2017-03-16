{-# LANGUAGE BangPatterns #-}
module Network.Socket.ByteString.Lazy.Windows
    (
    -- * Send data to a socket
      send
    , sendAll
    ) where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

import Network.Socket (Socket(..))
import qualified Network.Socket.ByteString as Socket

-- -----------------------------------------------------------------------------
-- Sending

send :: Socket        -- ^ Connected socket
     -> L.ByteString  -- ^ Data to send
     -> IO Int64      -- ^ Number of bytes sent
send sock s = do
  fromIntegral <$> case L.toChunks s of
      -- TODO: Consider doing nothing if the string is empty.
      []    -> Socket.send sock S.empty
      (x:_) -> Socket.send sock x

sendAll :: Socket        -- ^ Connected socket
        -> L.ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll sock bs'
