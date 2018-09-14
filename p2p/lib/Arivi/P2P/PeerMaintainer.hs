{-# LANGUAGE RecordWildCards #-}

module Arivi.P2P.PeerMaintainer
    ( fillQuotas
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Arivi.P2P.MessageHandler.HandlerTypes
import qualified Arivi.P2P.Kademlia.Types as KademliaTypes
import Arivi.P2P.PRT.Instance (getKNodes)
import Arivi.P2P.RPC.Types
import Arivi.P2P.RPC.SendOptions

import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)
-- import Control.Monad (unless, forever)
import Control.Lens

-- | fills up the peer list for resource. Since Options message is not for a specific resource, check after each invocation of sendOptions if the number of peers if less than required quota for any resource. Recursively keep calling till all the quotas have been satisfied.
-- | TODO: Logging
fillQuotas :: (HasP2PEnv env m r t rmsg pmsg) => Integer -> m ()
fillQuotas numPeers = forever $ do
    archivedMapTVar <- archived
    archivedMap <- liftIO (readTVarIO archivedMapTVar)
    filled <- liftIO $ isFilled archivedMap numPeers
    unless filled $ do
        res <- runExceptT $ getKNodes numPeers -- Repetition of peers
        -- liftIO $ threadDelay (40 * 1000000)
        case res of
            Left _ -> liftIO $ threadDelay (40 * 1000000)
            Right peers -> do
                peerNodeIds <- addPeerFromKademlia peers
                sendOptionsMessage peerNodeIds Options
                liftIO $ print "waiting"
                liftIO $ threadDelay (40 * 1000000)            
            -- fillQuotas numPeers

isFilledHelper ::Int -> [TVar [a]] -> IO Bool
isFilledHelper _ [] = return True
isFilledHelper minimumNodes l  = not <$> (fmap (any (< minimumNodes)) <$> mapM (fmap length <$> readTVarIO)) l

-- | Returns true if all the resources have met the minimumNodes quota and false otherwise
isFilled :: ArchivedResourceToPeerMap r msg -> Integer -> IO Bool
isFilled archivedMap minNodes = do
    let resourceToPeerList = HM.toList (getArchivedMap archivedMap)
    isFilledHelper (fromIntegral minNodes) (fmap (snd . snd) resourceToPeerList)

-- | add the peers returned by Kademlia to the PeerDetails HashMap
addPeerFromKademlia ::
        (HasNodeEndpoint m, MonadIO m)
    => [KademliaTypes.Peer]
    -> m [NodeId]
addPeerFromKademlia = mapM (\peer -> do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    addPeerFromKademliaHelper peer nodeIdMapTVar)

addPeerFromKademliaHelper ::
        (MonadIO m)
    => KademliaTypes.Peer
    -> TVar NodeIdPeerMap
    -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar =
    liftIO $
        atomically
            (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
                let _nodeId = fst $ KademliaTypes.getPeer peerFromKademlia
                    kadNodeEndPoint = snd $ KademliaTypes.getPeer peerFromKademlia
                    mapEntry = HM.lookup _nodeId nodeIdPeerMap
                    _ip = KademliaTypes.nodeIp kadNodeEndPoint
                    _udpPort = KademliaTypes.udpPort kadNodeEndPoint
                    _tcpPort = KademliaTypes.tcpPort kadNodeEndPoint
                case mapEntry of
                    Nothing -> do
                        defaultPeer <-
                            (& networkConfig .~ NetworkConfig {..}) <$>
                            defaultPeerDetails
                        newPeer <- newTVar defaultPeer
                        let newHashMap = HM.insert _nodeId newPeer nodeIdPeerMap
                        writeTVar nodeIdPeerMapTVar newHashMap
                    Just value -> do
                        oldPeerDetails <- readTVar value
                        let newDetails =
                                oldPeerDetails & networkConfig .~
                                NetworkConfig {..}
                        writeTVar value newDetails
                return _nodeId)
