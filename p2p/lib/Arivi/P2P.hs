{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Arivi.P2P
    ( module Arivi.P2P
    ) where

import Arivi.Network
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.Handler
import Arivi.P2P.Kademlia.LoadDefaultPeers
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.P2PEnv
import Arivi.P2P.PeerMaintainer
import Codec.Serialise

-- import Arivi.P2P.Kademlia.LoadReputedPeers
-- import Arivi.P2P.PRT.Instance (getAllReputedNodes)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM.TVar
import Control.Monad.Except

emptyNodeList :: [NodeId]
emptyNodeList = []

-- | Called by the service in Xoken core
initP2P ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Config.Config -> m ()
initP2P config = do
    liftIO $ putStrLn $ "Starting Arivi P2P.. "
    _ <- async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    _ <- async (runTcpServer (show (Config.tcpPort config)) newIncomingConnectionHandler)
    loadDefaultPeers (Config.trustedPeers config)
    liftIO $ threadDelay 5000000
    nl <- liftIO $ newTVarIO emptyNodeList
    _ <- async $ fillQuotas 2 nl -- hardcoding here assuming each resource requires at least 2 peers
    _ <- async $ maintainSubscriptions nl
    return ()
