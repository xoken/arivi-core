{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

module Arivi.Network.StreamServer
    ( runTcpServer
    ) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.ConnectionHandler (readHandshakeInitSock, establishSecureConnection, readTcpSock, sendTcpMessage, closeConnection)
import           Arivi.Network.StreamClient      (createFrame)
import qualified Arivi.Network.Connection        as Conn (socket)
import           Arivi.Network.Types             (ConnectionHandle (..))
import           Control.Concurrent.Async.Lifted (async)
import           Control.Exception.Lifted        (finally)
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.IORef                      (newIORef)
import           Data.HashMap.Strict             as HM (empty)
import           Network.Socket

-- Functions for Server
-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- | Creates server Thread that spawns new thread for listening.
runTcpServer ::
       (HasSecretKey m, HasLogging m)
    => ServiceName
    -> (ConnectionHandle -> m ())
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
        finally (acceptIncomingSocket sock handler) (liftIO $ Network.Socket.close sock)

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket ::
       (HasSecretKey m, HasLogging m)
    => Socket
    -> (ConnectionHandle -> m ())
    -> m ()
acceptIncomingSocket sock handler =
    forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        async (handleInboundConnection mSocket handler) --or use forkIO

-- TODO: Use rec MonadFix
handleInboundConnection ::
       (HasSecretKey m, HasLogging m)
    => Socket
    -> (ConnectionHandle -> m ())
    -> m ()
handleInboundConnection sock handler =
    $(withLoggingTH)
        (LogNetworkStatement "handleInboundConnection: ")
        LevelDebug $ do
        sk <- getSecretKey
        conn <- liftIO $ readHandshakeInitSock sock >>= establishSecureConnection sk sock createFrame
        fragmentsHM <- liftIO $ newIORef HM.empty
        handler
            ConnectionHandle
            { Arivi.Network.Types.send = sendTcpMessage conn
            , Arivi.Network.Types.recv = readTcpSock conn fragmentsHM
            , Arivi.Network.Types.close = closeConnection (Conn.socket conn)
            }
