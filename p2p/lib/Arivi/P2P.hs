{-# language Rank2Types, ScopedTypeVariables  #-}

module Arivi.P2P
    ( module Arivi.P2P
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.Handler
import Arivi.Network
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.PeerMaintainer
import Arivi.P2P.Kademlia.LoadDefaultPeers
-- import Arivi.P2P.Kademlia.LoadReputedPeers
-- import Arivi.P2P.PRT.Instance (getAllReputedNodes)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Monad.Except


-- | Called by the service in Xoken core
initP2P :: (HasP2PEnv env m r t rmsg pmsg) => Config.Config ->  m ()
initP2P config = do
    _ <- async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    _ <- async (runTcpServer (show (Config.tcpPort config)) newIncomingConnectionHandler)
    loadDefaultPeers (Config.trustedPeers config)
    liftIO $ threadDelay 5000000
    -- reputedNodes <- getAllReputedNodes
    -- loadReputedPeers reputedNodes -- What do I do with an error. Doing nothing for now
    _ <- async $ fillQuotas 5 -- hardcoding here assuming each resource requires 5 peers
    return ()
    -- wait tcpTid
    -- wait udpTid
