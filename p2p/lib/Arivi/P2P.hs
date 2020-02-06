<<<<<<< HEAD
{-# language Rank2Types, ScopedTypeVariables  #-}
=======
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P
    ( module Arivi.P2P
    ) where

<<<<<<< HEAD
import Arivi.P2P.P2PEnv
import Arivi.P2P.Handler
import Arivi.Network
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.PeerMaintainer
import Arivi.P2P.Kademlia.LoadDefaultPeers
import Arivi.P2P.Kademlia.LoadReputedPeers
import Arivi.P2P.PRT.Instance (getAllReputedNodes)
import Arivi.P2P.RPC.Types
import Arivi.P2P.RPC.Functions

import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted
import Control.Monad.Except


-- | Called by the service in Xoken core
initP2P :: (HasP2PEnv env m r t rmsg pmsg) => Config.Config -> HM.HashMap r (ResourceHandler r rmsg) ->  m ()
initP2P config resourceHandlers = do
=======
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
>>>>>>> breaking out arivi-core from arivi
    _ <- async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    _ <- async (runTcpServer (show (Config.tcpPort config)) newIncomingConnectionHandler)
    loadDefaultPeers (Config.trustedPeers config)
    liftIO $ threadDelay 5000000
<<<<<<< HEAD
    reputedNodes <- getAllReputedNodes
    loadReputedPeers reputedNodes -- What do I do with an error. Doing nothing for now
    mapM_ (\(resource, handler) -> registerResource resource handler Archived) (HM.toList resourceHandlers)
    _ <- async $ fillQuotas 5 -- hardcoding here assuming each resource requires 5 peers
    return ()
    -- wait tcpTid
    -- wait udpTid
=======
    nl <- liftIO $ newTVarIO emptyNodeList
    _ <- async $ fillQuotas 2 nl -- hardcoding here assuming each resource requires at least 2 peers
    _ <- async $ maintainSubscriptions nl
    return ()
>>>>>>> breaking out arivi-core from arivi
