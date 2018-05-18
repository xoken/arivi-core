module Arivi.Network.StreamServer
(
  readSock
  , runTCPserver
) where

import           Arivi.Network.FrameDispatcher (handleInboundConnection)
import           Arivi.Network.Types           (Parcel (..), deserialise)
import           Control.Concurrent            (forkFinally)
import           Control.Concurrent.Async
import           Control.Concurrent.STM        (STM, TChan, TMVar, atomically,
                                                newTChan, newTMVar, readTChan,
                                                readTMVar, writeTChan)
import           Control.Concurrent.STM.TChan
import           Control.Monad                 (forever, void)
import           Data.Binary
import qualified Data.ByteString               as BS
import           Data.ByteString.Internal      (unpackBytes)
import qualified Data.ByteString.Lazy          as BSL
import           Data.Int
import qualified Data.List.Split               as S
import           Data.Maybe                    (fromMaybe)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString     as N (recv, recvFrom)
--import           System.Posix.Unistd -- for testing only


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


readSock sock parcelTChan = forever $ do
        parcel <- getParcel sock
        atomically $ writeTChan parcelTChan parcel

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


