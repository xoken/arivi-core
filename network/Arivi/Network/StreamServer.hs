{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Arivi.Network.StreamServer
(
    readSock
  , runTCPserver
  , runUDPserver
  , readSockUDP
  , readSockUDP'
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.FrameDispatcher   (handleInboundConnection)
import           Arivi.Network.StreamClient
import           Arivi.Network.Types             (DeserialiseFailure,
                                                  Event (..), Header (..),
                                                  Parcel (..),
                                                  deserialiseOrFail)
import           Control.Concurrent              (threadDelay)
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
import qualified Network.Socket.ByteString       as N (recv, recvFrom)
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







runUDPserver :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => ServiceName -> m ()
runUDPserver portNumber =  $(withLoggingTH) (LogNetworkStatement "Server started...") LevelInfo $ do

    let hint = defaultHints {addrFlags = [AI_PASSIVE],
                               addrSocketType = Datagram}


    addr:_ <- liftIO $ getAddrInfo (Just hint) (Just "127.0.0.1") (Just portNumber)

    mSocket <- liftIO $  socket (addrFamily addr) (addrSocketType addr)
                                        (addrProtocol addr)
    liftIO $ print "runUDPserver 1"
    liftIO $  bind mSocket (addrAddress addr)
    liftIO $ print "runUDPserver 2"
    socketName <- liftIO $ getSocketName mSocket
    mIpAddress <- liftIO $ inet_ntoa $ getIPAddress socketName
    liftIO $ print mIpAddress
    void $ forkFinally (acceptIncomingSocketUDP mSocket) (\_ -> liftIO $ close mSocket)


-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocketUDP :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => Socket -> m a
acceptIncomingSocketUDP sock = do
  sk <- getSecretKey
  -- forever $ do
        -- msg <- liftIO $ recvFrom sock 4096
        -- liftIO $ putStrLn $ "Connection from " ++ show peer
        -- mSocket <- liftIO $ createSocketUDP "127.0.0.1" 4500 UDP
  eventTChan <- liftIO $ atomically newTChan
  -- _ <- async (handleInboundConnection sock eventTChan)  --or use forkIO
  _ <- async (readSockUDP' sock eventTChan sk)
  _ <- liftIO $ threadDelay 10000000
  return undefined


readSockUDP :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m)
            => Socket -> TChan Event -> SecretKey -> m ()
readSockUDP sock eventTChan sk = do
      (message,sockAddra) <- liftIO $ N.recvFrom sock 4096
      traceShow ("from sender message") (return ())
      traceShow (message) (return ())

      -- traceShow "before connect" (return ())
      -- liftIO $ connect sock (sockAddra)

      traceShow "from sender before handle" (return ())
      _ <- async (handleInboundConnection sock eventTChan)  --or use forkIO
      traceShow "from sender after handle" (return ())
      liftIO $ (getParcelUDP message >>=
             either (do traceShow "treceSHo error" (return())
                        sendFrame sock . BSLC.pack . displayException)
                    (\case
                        e@(Parcel (HandshakeInitHeader _ _) _) -> do
                          traceShow e (return ())
                          atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
                          traceShow "from sender  inside HandshakeInitHeader" (return ())
                        e@(Parcel (HandshakeRespHeader _ _) _) -> do
                         traceShow e (return ())
                         traceShow "from sender inside HandshakeRespHeader" (return ())
                         atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
                        e@(Parcel DataHeader {} _)    -> do
                          traceShow e (return ())
                          atomically $ writeTChan eventTChan (ReceiveDataEvent e)
                          traceShow "from sender  inside DataHeader" (return ())
                        e                                      -> do
                          traceShow e (return ())
                          traceShow "from sender inside error" (return ())
                          sendFrame sock "O traveler! Don't wander into uncharted territories!"
                    ))

      forever $
           liftIO $ ( N.recv sock 4096 >>=
            getParcelUDP >>=
            either (do traceShow "from sender treceSHo error" (return())
                       sendFrame sock . BSLC.pack . displayException)
                   (\case
                       e@(Parcel (HandshakeInitHeader _ _) _) -> do
                         traceShow e (return ())
                         atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
                         traceShow "from sender inside HandshakeInitHeader" (return ())
                       e@(Parcel (HandshakeRespHeader _ _) _) -> do
                        traceShow e (return ())
                        atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
                        traceShow "from sender inside HandshakeRespHeader" (return ())
                       e@(Parcel DataHeader {} _)    -> do
                         traceShow e (return ())
                         atomically $ writeTChan eventTChan (ReceiveDataEvent e)
                         traceShow "from sender inside DataHeader" (return ())
                       e                                      -> do
                         traceShow e (return ())
                         traceShow "from sender inside error" (return ())
                         sendFrame sock "O traveler! Don't wander into uncharted territories!"
                   ))

readSockUDP' :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m)
            => Socket -> TChan Event -> SecretKey -> m ()
readSockUDP' sock eventTChan sk = do
      (message,sockAddra) <- liftIO $ N.recvFrom sock 4096
      traceShow ("message") (return ())
      traceShow (message) (return ())

      traceShow "before connect" (return ())
      liftIO $ connect sock (sockAddra)

      traceShow "before handle" (return ())
      _ <- async (handleInboundConnection sock eventTChan)  --or use forkIO
      traceShow "after handle" (return ())
      liftIO $ (getParcelUDP message >>=
             either (do traceShow "treceSHo error" (return())
                        sendFrame sock . BSLC.pack . displayException)
                    (\case
                        e@(Parcel (HandshakeInitHeader _ _) _) -> do
                          traceShow e (return ())
                          atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
                          traceShow "inside HandshakeInitHeader" (return ())
                        e@(Parcel (HandshakeRespHeader _ _) _) -> do
                         traceShow e (return ())
                         traceShow "inside HandshakeRespHeader" (return ())
                         atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
                        e@(Parcel DataHeader {} _)    -> do
                          traceShow e (return ())
                          atomically $ writeTChan eventTChan (ReceiveDataEvent e)
                          traceShow "inside DataHeader" (return ())
                        e                                      -> do
                          traceShow e (return ())
                          traceShow "inside error" (return ())
                          sendFrame sock "O traveler! Don't wander into uncharted territories!"
                    ))
      forever $
           liftIO $ ( N.recv sock 4096 >>=
            getParcelUDP >>=
            either (do traceShow "treceSHo error" (return())
                       sendFrame sock . BSLC.pack . displayException)
                   (\case
                       e@(Parcel (HandshakeInitHeader _ _) _) -> do
                         traceShow e (return ())
                         atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
                         traceShow "inside HandshakeInitHeader" (return ())
                       e@(Parcel (HandshakeRespHeader _ _) _) -> do
                        traceShow e (return ())
                        atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
                        traceShow "inside HandshakeRespHeader" (return ())
                       e@(Parcel DataHeader {} _)    -> do
                         traceShow e (return ())
                         atomically $ writeTChan eventTChan (ReceiveDataEvent e)
                         traceShow "inside DataHeader" (return ())
                       e                                      -> do
                         traceShow e (return ())
                         sendFrame sock "O traveler! Don't wander into uncharted territories!"
                         traceShow "inside error" (return ())
                   ))

getParcelUDP :: _ -> IO (Either DeserialiseFailure Parcel)
getParcelUDP msg = do
    -- (parcelCipher,sockAddrs) <- N.recvFrom sock 4096
    -- (parcelCipher,sockAddrs) <- N.recvFrom sock $ getFrameLength lenbs
    print "fsd"
    -- traceShow msg (return ())
    return $ deserialiseOrFail (BSL.fromStrict msg)


getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet _ hostAddress) = hostAddress
getIPAddress _                            = error "getIPAddress: SockAddr is not of constructor SockAddrInet "
