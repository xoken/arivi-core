<<<<<<< HEAD
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
=======
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.Network.StreamServer
    ( runTcpServer
    ) where

<<<<<<< HEAD
import           Arivi.Env
import qualified Arivi.Network.Connection        as Conn (remoteNodeId, socket,
                                                          transportType, ipAddress,
                                                          port)
import           Arivi.Network.ConnectionHandler (closeConnection,
                                                  establishSecureConnection,
                                                  readHandshakeInitSock,
                                                  readTcpSock, sendTcpMessage)
import           Arivi.Network.StreamClient      (createFrame)
import           Arivi.Network.Types             (ConnectionHandle (..),
                                                  TransportType, NetworkConfig(..))
import           Arivi.Utils.Logging
import           Control.Concurrent.Async.Lifted (async)
import           Control.Exception.Lifted        (finally)
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.HashMap.Strict             as HM (empty)
import           Data.IORef                      (newIORef)
import           Network.Socket
=======
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
>>>>>>> breaking out arivi-core from arivi

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
<<<<<<< HEAD
    $(withLoggingTH) (LogNetworkStatement "TCP Server started...") LevelInfo $
    liftWithSocketsDo $ do
        let hints =
                defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        addr:_ <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
    -- TODO: Deal with socket exceptions
        sock <-
            liftIO $
            socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
=======
    liftWithSocketsDo $ do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        addrs <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
        -- liftIO $ putStrLn $ show addrs
        let addr = addrs !! 0
    -- TODO: Deal with socket exceptions
        sock <- liftIO $ socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
>>>>>>> breaking out arivi-core from arivi
        liftIO $ setSocketOption sock ReuseAddr 1
        liftIO $ setSocketOption sock ReusePort 1
        liftIO $ bind sock (addrAddress addr)
        liftIO $ listen sock 5
<<<<<<< HEAD
        finally
            (acceptIncomingSocket sock handler)
            (liftIO $ Network.Socket.close sock)
=======
        finally (acceptIncomingSocket sock handler) (liftIO $ Network.Socket.close sock)
>>>>>>> breaking out arivi-core from arivi

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket ::
<<<<<<< HEAD
       (HasSecretKey m, HasLogging m)
    => Socket
    -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ())
    -> m ()
=======
       (HasSecretKey m, HasLogging m) => Socket -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ()) -> m ()
>>>>>>> breaking out arivi-core from arivi
acceptIncomingSocket sock handler =
    forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        async (handleInboundConnection mSocket handler) --or use forkIO

-- TODO: Use rec MonadFix
handleInboundConnection ::
<<<<<<< HEAD
       (HasSecretKey m, HasLogging m)
    => Socket
    -> (NetworkConfig -> TransportType -> ConnectionHandle -> m ())
    -> m ()
handleInboundConnection sock handler =
    $(withLoggingTH)
        (LogNetworkStatement "handleInboundConnection: ")
        LevelDebug $ do
        sk <- getSecretKey
        conn <-
            liftIO $
            readHandshakeInitSock sock >>=
            establishSecureConnection sk sock createFrame
        fragmentsHM <- liftIO $ newIORef HM.empty
        let nc =
                NetworkConfig
                { _nodeId = Conn.remoteNodeId conn
                , _ip = Conn.ipAddress conn
                , _udpPort = Conn.port conn
                , _tcpPort = 0
                }
        handler
            nc
            (Conn.transportType conn)
            ConnectionHandle
=======
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
>>>>>>> breaking out arivi-core from arivi
            { Arivi.Network.Types.send = sendTcpMessage conn
            , Arivi.Network.Types.recv = readTcpSock conn fragmentsHM
            , Arivi.Network.Types.close = closeConnection (Conn.socket conn)
            }
