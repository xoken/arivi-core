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
import Arivi.P2P.RPC.Types
import Arivi.P2P.RPC.Functions

import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Monad.Reader

-- | Called by the service in Xoken core
initP2P :: (HasP2PEnv env m r msg) => Config.Config -> HM.HashMap r (ResourceHandler r msg) -> ReaderT (P2PEnv r msg) m ()
initP2P config resourceHandlers = do
    udpTid <- lift $ async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    -- tcpTid <- lift $ async (runTcpServer (show (Config.tcpPort config)) newIncomingConnectionHandler)
    lift $ loadDefaultPeers (Config.trustedPeers config)
    lift $ mapM_ (\(resource, handler) -> registerResource resource handler Archived) (HM.toList resourceHandlers)
    --call loadReputedPeers here after fetching reputed peers from PRT
    lift $ fillQuotas 5 -- hardcoding here assuming each resource requires 5 peers
    lift $ wait udpTid
    -- lift $ wait tcpTid
