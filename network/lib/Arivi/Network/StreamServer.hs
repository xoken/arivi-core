{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

module Arivi.Network.StreamServer
(
  runTCPServer
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.ConnectionHandler (handleInboundConnection)
import           Arivi.Network.Types             (ConnectionHandle)
import           Control.Concurrent.Async.Lifted (async)
import           Control.Exception.Lifted        (finally)
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Network.Socket

-- Functions for Server
-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- | Creates server Thread that spawns new thread for listening.
runTCPServer ::
       (HasSecretKey m, HasLogging m)
    => ServiceName
    -> (ConnectionHandle -> m ())
    -> m ()
runTCPServer port handler =
    $(withLoggingTH) (LogNetworkStatement "Server started...") LevelInfo $
    liftWithSocketsDo $ do
        let hints =
                defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
        addr:_ <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
    -- TODO: Deal with socket exceptions
        sock <-
            liftIO $
            socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        liftIO $ setSocketOption sock ReuseAddr 1
        liftIO $ bind sock (addrAddress addr)
        liftIO $ listen sock 5
        finally (acceptIncomingSocket sock handler) (liftIO $ close sock)

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
