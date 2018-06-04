{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Arivi.Network.StreamServer
(
    readSock
  , runTCPserver
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.FrameDispatcher   (handleInboundConnection)
import           Arivi.Network.StreamClient
import           Arivi.Network.Types             (DeserialiseFailure,
                                                  Event (..), Header (..),
                                                  Parcel (..),
                                                  deserialiseOrFail)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted       (forkFinally)
import           Control.Concurrent.STM          (TChan, atomically, newTChan,
                                                  writeTChan)
import           Control.Exception.Base
import           Control.Monad                   (forever, void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Crypto.PubKey.Ed25519           (SecretKey)
import           Data.Binary
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy.Char8      as BSLC
import           Data.Int
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString       as N (recv)

-- Functions for Server

-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- | Creates server Thread that spawns new thread for listening.
--runTCPserver :: String -> TChan Socket -> IO ()
runTCPserver :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => ServiceName -> m ()
runTCPserver port = $(withLoggingTH) (LogNetworkStatement "Server started...") LevelInfo $
  liftWithSocketsDo $ do
    let hints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream  }
    addr:_ <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
    -- TODO: Deal with socket exceptions
    sock <- liftIO $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    liftIO $ setSocketOption sock ReuseAddr 1
    liftIO $ bind sock (addrAddress addr)
    liftIO $ listen sock 5
    void $ forkFinally (acceptIncomingSocket sock) (\_ -> liftIO $ close sock)

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => Socket -> m a
acceptIncomingSocket sock = do
  sk <- getSecretKey
  forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        eventTChan <- liftIO $ atomically newTChan
        _ <- async (handleInboundConnection mSocket eventTChan)  --or use forkIO
        async (liftIO $ readSock mSocket eventTChan sk)


-- | Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either DeserialiseFailure Parcel)
getParcel sock = do
    lenbs <- N.recv sock 2
    parcelCipher <- N.recv sock $ getFrameLength lenbs
    return $ deserialiseOrFail (BSL.fromStrict parcelCipher)


readSock :: Socket -> TChan Event -> SecretKey -> IO ()
readSock sock eventTChan sk = forever $
        getParcel sock >>=
        either (sendFrame sock . BSLC.pack . displayException)
               (\case
                   e@(Parcel (HandshakeInitHeader _ _) _) -> do
                     traceShow e (return ())
                     atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
                   e@(Parcel (HandshakeRespHeader _ _) _) -> do
                    traceShow e (return ())
                    atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
                   e@(Parcel DataHeader {} _)    -> do
                     traceShow e (return ())
                     atomically $ writeTChan eventTChan (ReceiveDataEvent e)
                   e                                      -> do
                     traceShow e (return ())
                     sendFrame sock "O traveller! Don't wander into uncharted terriories!"
               )

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
--    -- (sock,eventTChan) <- atomically $ readTChan sockTChan
--     async (readSock sock eventTChan)
--     --putStrLn ("listening on thread " ++  (show threadNo) )
--     where readSock sock eventTChan = forever $ do
--                 parcelCipher <- getFrame sock
--                 atomically $ writeTChan eventTChan parcelCipher
