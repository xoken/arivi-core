{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Arivi.Network.StreamServer
    ( runTcpServer
    ) where

import Arivi.Env
import qualified Arivi.Network.Connection as Conn (ipAddress, port, remoteNodeId, socket, transportType)
import Arivi.Network.ConnectionHandler
    ( closeConnection
    , establishSecureConnection
    , readHandshakeInitSock
    , readTcpSock
    , sendTcpMessage
    )
import Arivi.Network.StreamClient (createFrame)
import Arivi.Network.Types (ConnectionHandle(..), NetworkConfig(..), TransportType)
import Arivi.Utils.Logging
import Control.Concurrent.Async.Lifted (async)
import Control.Exception.Lifted (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.HashMap.Strict as HM (empty)
import Data.IORef (newIORef)
import Network.Socket

-- Functions for Server
-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- | Creates server Thread that spawns new thread for listening.
runTcpServer ::
       (HasSecretKey m, HasLogging m)
    => ServiceName
    -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ())
    -> m ()
runTcpServer port handler =
    liftWithSocketsDo $ do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        addrs <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
        -- liftIO $ putStrLn $ show addrs
        let addr = addrs !! 0
    -- TODO: Deal with socket exceptions
        sock <- liftIO $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        liftIO $ setSocketOption sock ReuseAddr 1
        liftIO $ setSocketOption sock ReusePort 1
        liftIO $ bind sock (addrAddress addr)
        liftIO $ listen sock 5
        finally (acceptIncomingSocket sock handler) (liftIO $ Network.Socket.close sock)

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket ::
       (HasSecretKey m, HasLogging m) => Socket -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ()) -> m ()
acceptIncomingSocket sock handler =
    forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        async (handleInboundConnection mSocket handler) --or use forkIO

-- TODO: Use rec MonadFix
handleInboundConnection ::
       (HasSecretKey m, HasLogging m) => Socket -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ()) -> m ()
handleInboundConnection sock handler = do
    sk <- getSecretKey
    conn <- liftIO $ readHandshakeInitSock sock >>= establishSecureConnection sk sock createFrame
    fragmentsHM <- liftIO $ newIORef HM.empty
    let nc =
            NetworkConfig
                {_nodeId = Conn.remoteNodeId conn, _ip = Conn.ipAddress conn, _udpPort = Conn.port conn, _tcpPort = 0}
    handler
        nc
        (Conn.transportType conn)
        ConnectionHandle
            { Arivi.Network.Types.send = sendTcpMessage conn
            , Arivi.Network.Types.recv = readTcpSock conn fragmentsHM
            , Arivi.Network.Types.close = closeConnection (Conn.socket conn)
            }
