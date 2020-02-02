{-# LANGUAGE RecordWildCards #-}

module Arivi.P2P.PeerMaintainer
    ( fillQuotas
    , maintainSubscriptions
    ) where

import qualified Arivi.P2P.Kademlia.Types as KademliaTypes
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.P2PEnv
import Arivi.P2P.PRT.Instance (getKNodes)
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Subscribe
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.SendOptions
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.Set as Set

-- | Sends subscribe messages for each topic to every passed peer.
-- | TODO: Batched subscribes for multiple topics.
maintainSubscriptions ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           TVar [NodeId] -> m ()
maintainSubscriptions nodeTV =
    forever $ do
        topicVar <- asks topics
        topicList <- liftIO $ atomically $ readTVar topicVar
        nodeList <- liftIO $ atomically $ readTVar nodeTV
        liftIO $ print (length topicList)
        mapM_ (subscribeForTopic nodeList) (Set.toList topicList)
        liftIO $ threadDelay (30 * 1000000)

subscribeForTopic ::
       (Serialise msg, Show t)
    => (HasP2PEnv env m r t rmsg msg) =>
           [NodeId] -> t -> m ()
subscribeForTopic nodeList t = mapConcurrently_ (subscribe (PubSubPayload (t, 100000))) nodeList
    -- Hardcoding timer value for now. No logic for handling it currently.
    -- Subscriptions are for the duration of a network connection as of now

-- | fills up the peer list for resource. Since Options message is not for a specific resource,
-- | check after each invocation of sendOptions if the number of peers if less than required quota for any resource. Recursively keep calling till all the quotas have been satisfied.
-- | TODO: Logging
fillQuotas ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Integer -> TVar [NodeId] -> m ()
fillQuotas numPeers nodeTV =
    forever $ do
        rpcRecord <- asks rpcEnv
        let Resourcers resourcers = rpcResourcers rpcRecord
        filledResources <- liftIO $ isFilled resourcers numPeers
        Notifiers _notif <- asks notifiers
  -- _               <- liftIO $ isFilled notif numPeers
        unless filledResources $ do
            res <- runExceptT $ getKNodes numPeers -- Repetition of peers
            case res of
                Left _ -> liftIO $ threadDelay (30 * 1000000)
                Right peers -> do
                    peerNodeIds <- addPeerFromKademlia peers
                    sendOptionsMessage peerNodeIds Options
                    nodeList <- liftIO $ atomically $ readTVar nodeTV
                    let newNodeList = Set.toList (Set.fromList (nodeList ++ peerNodeIds))
                    liftIO $ atomically $ writeTVar nodeTV newNodeList
                    liftIO $ threadDelay (30 * 1000000)

isFilledHelper :: Int -> [TVar (Set a)] -> IO Bool
isFilledHelper _ [] = return True
isFilledHelper minimumNodes l = not <$> (fmap (any (< minimumNodes)) <$> mapM (fmap Set.size <$> readTVarIO)) l

-- | Returns true if all the resources have met the minimumNodes quota and false otherwise
isFilled :: HM.HashMap a (TVar (Set b)) -> Integer -> IO Bool
isFilled hm minNodes = do
    let l = HM.toList hm
    isFilledHelper (fromIntegral minNodes) (fmap snd l)

-- | add the peers returned by Kademlia to the PeerDetails HashMap
addPeerFromKademlia :: (HasNodeEndpoint m, MonadIO m) => [KademliaTypes.Peer] -> m [NodeId]
addPeerFromKademlia =
    mapM
        (\peer -> do
             nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
             addPeerFromKademliaHelper peer nodeIdMapTVar)

addPeerFromKademliaHelper :: (MonadIO m) => KademliaTypes.Peer -> TVar NodeIdPeerMap -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar =
    liftIO $
    atomically
        (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
            let _nodeId = KademliaTypes.nodeID peerFromKademlia
                kadNodeEndPoint = KademliaTypes.nodeEndPoint peerFromKademlia
                mapEntry = HM.lookup _nodeId nodeIdPeerMap
                _ip = KademliaTypes.nodeIp kadNodeEndPoint
                _udpPort = KademliaTypes.udpPort kadNodeEndPoint
                _tcpPort = KademliaTypes.tcpPort kadNodeEndPoint
            case mapEntry of
                Nothing -> do
                    defaultPeer <- (& networkConfig .~ NetworkConfig {..}) <$> defaultPeerDetails
                    newPeer <- newTVar defaultPeer
                    let newHashMap = HM.insert _nodeId newPeer nodeIdPeerMap
                    writeTVar nodeIdPeerMapTVar newHashMap
                Just value -> do
                    oldPeerDetails <- readTVar value
                    let newDetails = oldPeerDetails & networkConfig .~ NetworkConfig {..}
                    writeTVar value newDetails
            return _nodeId)
