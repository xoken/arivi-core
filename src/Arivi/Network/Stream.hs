module Arivi.Network.Stream
(
    runTCPServerForever
) where

import           Control.Concurrent        (ThreadId, forkIO, newEmptyMVar,
                                            putMVar, takeMVar, forkFinally)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM    (TChan, TMVar, atomically, newTChan,
                                            newTMVar, readTChan, readTMVar,
                                            writeTChan, STM)
import           Control.Monad             (forever,void)
import qualified Data.ByteString.Char8   as C
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString         as BS
import           Data.ByteString.Internal (unpackBytes)
import qualified Data.List.Split         as S
import           Data.Maybe                (fromMaybe)
import           Data.Word
import           Network.Socket hiding (recv)
import qualified Network.Socket.ByteString as N (recvFrom, sendTo, recv, sendAll)
import           Data.Binary
import           Data.Int


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
    N.sendAll sock databs


-- | creates frame(prefixes length) from parcel
-- that has been serialised to cborg
createFrame :: BSL.ByteString -> BSL.ByteString
createFrame parcelSerialised = BSL.concat [lenSer, parcelSerialised]
                                where
                      len = BSL.length parcelSerialised
                      lenw16 = (fromIntegral len) :: Int16
                      lenSer = encode lenw16






-- Functions for Server
-- NEEDS CHANGE : FOREVER LOOP TO BE ADDED
--runTCPserver :: String -> IO Socket
runTCPserverFor port inboundTChan = withSocketsDo $ do
    let hints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream  }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 5
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn inboundTChan) (\_ -> close conn)
        where
        talk conn inboundTChan = do
            parcelCipher <- getFrame conn
            atomically $ writeTChan inboundTChan (conn,parcelCipher)
            talk conn inboundTChan



-- Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Reads frame a given socket
getFrame :: Socket -> IO BSL.ByteString
getFrame sock = do
    --(conn, peer) <- accept sock
    lenbs <- N.recv sock 2
    parcelCipher <- N.recv sock $ getFrameLength lenbs
    let parcelCipherLazy = BSL.pack $ unpackBytes parcelCipher
    return parcelCipherLazy






-- FOR TESTING ONLY------
sampleParcel :: String -> BSL.ByteString
sampleParcel msg = createFrame b
                    where
                        s = unpackBytes $ C.pack msg
                        b = BSL.pack s
sendSample:: String -> IO()
sendSample msgStr = do
    soc <- getSocket "127.0.0.1" 3000 TCP
    let msg = sampleParcel msgStr
    sendByteTo soc (BSL.toStrict msg)

--test :: Socket -> IO (Socket, BSL.ByteString)
test = do
    let a = newTChan ::STM (TChan (Socket,BSL.ByteString))
    b <- atomically $ a
    --soc <- getSocket "127.0.0.1" 3516 TCP
    --atomically $ writeTChan b (sock, (sampleParcel "text"))
    --f <- atomically $ readTChan b
    putStrLn "Starting Server..."
    runTCPserverFor "3000" b
    --return f
