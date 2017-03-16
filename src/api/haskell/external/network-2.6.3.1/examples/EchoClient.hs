-- Echo client program
module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       connect sock (addrAddress serveraddr)
       sendAll sock $ C.pack "Hello, world!"
       msg <- recv sock 1024
       close sock
       putStr "Received "
       C.putStrLn msg
