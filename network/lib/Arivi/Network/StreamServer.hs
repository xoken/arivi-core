{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Arivi.Network.StreamServer
    ( runTcpServer
    ) where

import           Arivi.Env
import qualified Arivi.Network.Connection        as Conn (remoteNodeId, socket,
                                                          transportType)
import           Arivi.Network.ConnectionHandler (closeConnection,
                                                  establishSecureConnection,
                                                  readHandshakeInitSock,
                                                  readTcpSock, sendTcpMessage)
import           Arivi.Network.StreamClient      (createFrame)
import           Arivi.Network.Types             (ConnectionHandle (..), NodeId,
                                                  TransportType)
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Concurrent.Async.Lifted (async)
import           Control.Exception.Lifted        (finally)
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.HashMap.Strict             as HM (empty)
import           Data.IORef                      (newIORef)
import           Data.Time.Units
import           Network.Socket
import           System.CPUTime



-- Functions for Server
-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- | Creates server Thread that spawns new thread for listening.
runTcpServer ::
       (HasSecretKey m, HasLogging m,HasStatsdClient m)
    => ServiceName
    -> (NodeId -> TransportType -> ConnectionHandle -> m ())
    -> m ()
runTcpServer port handler =
    $(withLoggingTH) (LogNetworkStatement "TCP Server started...") LevelInfo $
    liftWithSocketsDo $ do
        let hints =
                defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        addr:_ <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
    -- TODO: Deal with socket exceptions
        sock <-
            liftIO $
            socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        liftIO $ setSocketOption sock ReuseAddr 1
        liftIO $ setSocketOption sock ReusePort 1
        liftIO $ bind sock (addrAddress addr)
        liftIO $ listen sock 5
        finally
            (acceptIncomingSocket sock handler)
            (liftIO $ Network.Socket.close sock)

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket ::
       (HasSecretKey m, HasLogging m,HasStatsdClient m)
    => Socket
    -> (NodeId -> TransportType -> ConnectionHandle -> m ())
    -> m ()
acceptIncomingSocket sock handler =
    forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        async (handleInboundConnection mSocket handler) --or use forkIO

-- TODO: Use rec MonadFix
handleInboundConnection ::
       (HasSecretKey m, HasLogging m, HasStatsdClient m)
    => Socket
    -> (NodeId -> TransportType -> ConnectionHandle -> m ())
    -> m ()
handleInboundConnection sock handler =
    $(withLoggingTH)
        (LogNetworkStatement "handleInboundConnection: ")
        LevelDebug $ do
        incrementCounter "Incoming Connection Attempted"
        initTime <- liftIO getCPUTime
        sk <- getSecretKey
        conn <-
            liftIO $
            readHandshakeInitSock sock >>=
            establishSecureConnection sk sock createFrame
        fragmentsHM <- liftIO $ newIORef HM.empty
        handler (Conn.remoteNodeId conn) (Conn.transportType conn)
            ConnectionHandle
            { Arivi.Network.Types.send = sendTcpMessage conn
            , Arivi.Network.Types.recv = readTcpSock conn fragmentsHM
            , Arivi.Network.Types.close = closeConnection (Conn.socket conn)
            }
        finalTime <- liftIO getCPUTime
        let diff = (fromIntegral ( (finalTime - initTime) `div` (1000000000)))
        time "Connection establishment time " $ (diff ::Millisecond)
        incrementCounter "Incoming Connection Established"

