{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where
import           Arivi.Env
import           Arivi.P2P.Types (NetworkConfig)
import           Arivi.P2P.Kademlia.Types              (createKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PRT.Types
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Data.HashMap.Strict                   as HM
import           Data.Ratio                            (Rational, (%))
import           Network.Socket                        (PortNumber)

data P2PEnv = P2PEnv
    { selfNId :: T.NodeId
    , tvarAriviP2PInstance :: TVar NetworkConfig
    , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
    , tvarArchivedResourceToPeerMap :: TVar ArchivedResourceToPeerMap
    , kbucket :: T.Kbucket Int [T.Peer]
    , statsdClient :: StatsdClient
    , tvarMessageTypeMap :: forall m. (HasP2PEnv m, HasLogging m) =>
                                          (MessageTypeMap m)
    , tvarWatchersTable :: TVar WatchersTable
    , tvarNotifiersTable :: TVar NotifiersTable
    , tvarTopicHandlerMap :: TVar TopicHandlerMap
    , tvarMessageHashMap :: TVar MessageHashMap
    , ariviNetworkEnv :: AriviEnv
    , tvarDynamicResourceToPeerMap :: TVar TransientResourceToPeerMap
    , tvPeerReputationHashTable :: TVar PeerReputationHistoryTable
    , tvServicesReputationHashMap :: TVar ServicesReputationHashMap
    , tvP2PReputationHashMap :: TVar P2PReputationHashMap
    , tvReputedVsOther :: TVar Rational
    , tvKClosestVsRandom :: TVar Rational
    }

class (T.HasKbucket m, HasStatsdClient m, HasNetworkEnv m, HasSecretKey m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getSelfNodeId :: m T.NodeId
    getAriviTVarP2PEnv :: m (TVar NetworkConfig)
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getArchivedResourceToPeerMapP2PEnv :: m (TVar ArchivedResourceToPeerMap)
    getMessageTypeMapP2PEnv :: m (MessageTypeMap m)
    getWatcherTableP2PEnv :: m (TVar WatchersTable)
    getNotifiersTableP2PEnv :: m (TVar NotifiersTable)
    getTopicHandlerMapP2PEnv :: m (TVar TopicHandlerMap)
    getMessageHashMapP2PEnv :: m (TVar MessageHashMap)
    getTransientResourceToPeerMap :: m (TVar TransientResourceToPeerMap)
    getPeerReputationHistoryTableTVar :: m (TVar PeerReputationHistoryTable)
    getServicesReputationHashMapTVar :: m (TVar ServicesReputationHashMap)
    getP2PReputationHashMapTVar :: m (TVar P2PReputationHashMap)
    getReputedVsOtherTVar :: m (TVar Rational)
    getKClosestVsRandomTVar :: m (TVar Rational)

makeP2PEnvironment ::
       String
    -> T.NodeId
    -> PortNumber
    -> PortNumber
    -> Int
    -> Int
    -> Int
    -> IO P2PEnv
makeP2PEnvironment nIp nId tPort uPort sbound pingThreshold kademliaConcurrencyFactor = do
    nmap <- newTVarIO HM.empty
    r2pmap <- newTVarIO HM.empty
    dr2pmap <- newTVarIO HM.empty
    kb <-
        createKbucket
            (T.Peer (nId, T.NodeEndPoint nIp tPort uPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor
    let mtypemap = HM.empty
    watcherMap <- newTVarIO HM.empty
    notifierMap <- newTVarIO HM.empty
    topicHandleMap <- newTVarIO HM.empty
    messageMap <- newTVarIO HM.empty
    peerReputationHashTable <- newTVarIO HM.empty
    servicesReputationHashMapTVar <- newTVarIO HM.empty
    p2pReputationHashMapTVar <- newTVarIO HM.empty
    reputedVsOtherTVar <- newTVarIO (1 % 1 :: Rational)
    kClosestVsRandomTVar <- newTVarIO (1 % 1 :: Rational)
    return
        { selfNId = nId
        , tvarNodeIdPeerMap = nmap
        , tvarArchivedResourceToPeerMap = r2pmap
        , tvarMessageTypeMap = mtypemap
        , tvarWatchersTable = watcherMap
        , tvarNotifiersTable = notifierMap
        , tvarTopicHandlerMap = topicHandleMap
        , tvarMessageHashMap = messageMap
        , tvarDynamicResourceToPeerMap = dr2pmap
        , kbucket = kb
        , kademliaConcurrencyFactor = alpha
        , kademliaSoftBound = sbound
        , tvPeerReputationHashTable = peerReputationHashTable
        , tvServicesReputationHashMap = servicesReputationHashMapTVar
        , tvP2PReputationHashMap = p2pReputationHashMapTVar
        , tvReputedVsOther = reputedVsOtherTVar
        , tvKClosestVsRandom = kClosestVsRandomTVar
        }
