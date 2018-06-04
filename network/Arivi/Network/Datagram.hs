module Arivi.Network.Datagram
(
    createUDPSocket
  , runUDPServerForever
) where

import           Control.Concurrent        (ThreadId, forkIO, newEmptyMVar,
                                            putMVar, takeMVar)
import           Control.Monad             (forever)
import           Network.Socket
import qualified Network.Socket.ByteString as N (recvFrom)

runUDPServerForever :: Socket
                    -> SockAddr
                    -> IO ()

runUDPServerForever sock sockAddr  = do

    bind sock sockAddr
    print ("UDP Server now listening for requests at : " ++ show sockAddr)
    forever $
                do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print ""

createUDPSocket :: Show portNumber => HostName -> portNumber -> IO Socket
createUDPSocket ipAddress portNumber = do
    let hint = defaultHints {addrFlags = [AI_PASSIVE],
                             addrSocketType = Datagram}

    selfAddr:_  <- getAddrInfo (Just hint) (Just ipAddress) (Just (show portNumber))
    mSocket <- socket (addrFamily selfAddr) (addrSocketType selfAddr)
                                        (addrProtocol selfAddr)
    bind mSocket (addrAddress selfAddr)
    return mSocket
