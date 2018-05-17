module Arivi.Network.Stream
(
    createSocket
  , readSock
  , runTCPserver
  , sendFrame
) where

import           Control.Concurrent           (ThreadId, forkFinally, forkIO,
                                               newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM       (STM, TChan, TMVar, atomically,
                                               newTChan, newTMVar, readTChan,
                                               readTMVar, writeTChan)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (forever, void)
import           Data.Binary
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C
import           Data.ByteString.Internal     (unpackBytes)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Int
import qualified Data.List.Split              as S
import           Data.Maybe                   (fromMaybe)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString    as N (recv, recvFrom, sendAll,
                                                    sendTo)
import           Arivi.Network.FrameDispatcher (handleInboundConnection)
import           Arivi.Network.Types (Parcel(..),deserialise)
--import           System.Posix.Unistd -- for testing only

-- Functions for Client connecting to Server

data TransportType =  UDP|TCP deriving(Eq)

getAddressType:: TransportType -> SocketType
getAddressType  transportType = if transportType==TCP then
                                Stream else Datagram

-- | Eg: createSocket "127.0.0.1" 3000 TCP
createSocket :: String -> Int -> TransportType -> IO Socket
createSocket ipAdd port transportType = withSocketsDo $ do
    let portNo = Just (show port)
    let transport_type = getAddressType transportType
    let hints = defaultHints {addrSocketType = transport_type}
    addr:_ <- getAddrInfo (Just hints) (Just ipAdd) portNo
    sock <- socket AF_INET transport_type (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

sendFrame :: Socket -> BSL.ByteString -> IO ()
sendFrame sock msg = N.sendAll sock (BSL.toStrict msg)

-- | prefixes length to cborg serialised parcel
createFrame :: BSL.ByteString -> BSL.ByteString
createFrame parcelSerialised = BSL.concat [lenSer, parcelSerialised]
                    where
                      len = BSL.length parcelSerialised
                      lenw16 = fromIntegral len :: Int16
                      lenSer = encode lenw16


-- Functions for Server

-- | Creates server Thread that spawns new thread for listening.
--runTCPserver :: String -> TChan Socket -> IO ()
runTCPserver port = withSocketsDo $ do
    let hints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream  }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 5
    void $ forkFinally (acceptIncomingSocket sock) (\_ -> close sock)
    putStrLn "Server started..."

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan

acceptIncomingSocket sock = forever $ do
        (socket, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        parcelTChan <- atomically newTChan
        async (handleInboundConnection socket parcelTChan)  --or use forkIO
        async (readSock sock parcelTChan)
        acceptIncomingSocket sock


-- | Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Reads frame a given socket
getParcel :: Socket -> IO Parcel
getParcel sock = do
    lenbs <- N.recv sock 2
    parcelCipher <- N.recv sock $ getFrameLength lenbs
    let parcelCipherLazy = BSL.pack $ unpackBytes parcelCipher
    return (deserialise parcelCipherLazy :: Parcel)



-- FOR TESTING ONLY------
{-
sampleParcel :: String -> BSL.ByteString
sampleParcel msg = createFrame b
                    where
                        s = unpackBytes $ C.pack msg
                        b = BSL.pack s

-- sendSample:: String -> IO()
sendMsgFromclient msg = do
    sock <- createSocket "127.0.0.1" 3000 TCP
    -- bsParcel <- getFrame sock
    --putStrLn $ "Recieved : " ++ (show bsParcel)
    sendFrame sock (sampleParcel msg)


--test :: Socket -> IO (Socket, BSL.ByteString)
test = do
    let parcelQ = newTChan :: STM (TChan BSL.ByteString)
    let sockQ = newTChan :: STM (TChan (Socket,parcelQ) )
    sampleTchan <- atomically $ sockQ
    putStrLn "Starting Server..."
    runTCPserver "3000" sampleTchan
    forkIO (readerLoop sampleTchan)
-}

-- readerLoop sock = forever $ do
--    -- (sock,parcelTChan) <- atomically $ readTChan sockTChan
--     async (readSock sock parcelTChan)
--     --putStrLn ("listening on thread " ++  (show threadNo) )
--     where readSock sock parcelTChan = forever $ do
--                 parcelCipher <- getFrame sock
--                 atomically $ writeTChan parcelTChan parcelCipher

readSock sock parcelTChan = forever $ do
        parcel <- getParcel sock
        atomically $ writeTChan parcelTChan parcel
