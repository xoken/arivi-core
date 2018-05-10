module Arivi.Network.Stream
(
    runTCPServerForever
) where

import           Control.Concurrent        (ThreadId, forkIO, newEmptyMVar,
                                            putMVar, takeMVar)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM    (TChan, TMVar, atomically, newTChan,
                                            newTMVar, readTChan, readTMVar,
                                            writeTChan)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import qualified Data.List.Split           as S
import           Data.Maybe                (fromMaybe)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString as N (recvFrom, sendTo)


runTCPServerForever :: Socket
                    -> SockAddr
                    -> IO ()

runTCPServerForever sock sockAddr = do
    bind sock sockAddr
    listen sock 3
    --  int above specifies the maximum number of queued connections and should
    --  be at least 1; the maximum value is system-dependent (usually 5).

    print "TCP Server now listening for requests at : " -- ++ show sockAddr)
    forever $
         do
            (conn,addr) <- accept sock
            (mesg, socaddr2) <- N.recvFrom sock 4096
            close conn
            close sock




-- Functions for Client connecting to Server


data TransportType =  UDP|TCP deriving(Eq)

getAddressType:: TransportType -> SocketType
getAddressType  transportType = if transportType==TCP then
                                Stream else Datagram

-- Example getSocket "127.0.0.1" 3000 TCP
getSocket :: String -> Int -> TransportType -> IO Socket
getSocket ipAdd port transportType = withSocketsDo $ do
    let portNo = Just (show port)
    let transport_type = (getAddressType transportType)
    let hints = defaultHints {addrSocketType = transport_type}
    addr:_ <- getAddrInfo (Just hints) (Just ipAdd) portNo
    sock <- socket AF_INET transport_type (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock


sendByteTo :: Socket -> C.ByteString -> IO ()
sendByteTo sock databs = do
    sendAll sock databs



-- Converts length in byteString to Num
getFrameLength :: Num b => S.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- Reads frame a given socket
getFrame :: Socket -> IO S.ByteString
getFrame sock = do
    lenbs <- recv conn 2
    parcelCipher <- recv conn $ getFrameLength lenbs
    return parcelCipher
