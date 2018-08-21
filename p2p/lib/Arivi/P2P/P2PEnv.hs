{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where

import           Arivi.Env
import           Arivi.P2P.Kademlia.Types              (PayLoad, createKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PRT.Types
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.Types                       (NetworkConfig (..),
                                                        Request, Resource,
                                                        Response, RpcPayload)
import           Arivi.P2P.Types                       (NetworkConfig (..))
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Codec.Serialise
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Lens.TH
import           Data.HashMap.Strict                   as HM
import           Data.Ratio                            (Rational, (%))
import           Network.Socket                        (PortNumber)

data P2PEnv = P2PEnv
    { _networkConfig                :: NetworkConfig
    , tvarNodeIdPeerMap             :: TVar NodeIdPeerMap
    , tvarArchivedResourceToPeerMap :: TVar ArchivedResourceToPeerMap
    , kbucket                       :: T.Kbucket Int [T.Peer]
    , statsdClient                  :: StatsdClient
    , tvarMessageTypeMap            :: Handlers
    , tvarWatchersTable             :: TVar WatchersTable
    , tvarNotifiersTable            :: TVar NotifiersTable
    , tvarTopicHandlerMap           :: TVar TopicHandlerMap
    , tvarMessageHashMap            :: TVar MessageHashMap
    , ariviNetworkEnv               :: AriviEnv
    , tvarDynamicResourceToPeerMap  :: TVar TransientResourceToPeerMap
    , tvPeerReputationHashTable     :: TVar PeerReputationHistoryTable
    , tvServicesReputationHashMap   :: TVar ServicesReputationHashMap
    , tvP2PReputationHashMap        :: TVar P2PReputationHashMap
    , tvReputedVsOther              :: TVar Rational
    , tvKClosestVsRandom            :: TVar Rational
    }

class (T.HasKbucket m, HasStatsdClient m, HasNetworkEnv m, HasSecretKey m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getAriviTVarP2PEnv :: m NetworkConfig
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getArchivedResourceToPeerMapP2PEnv :: m (TVar ArchivedResourceToPeerMap)
    getMessageTypeMapP2PEnv :: m Handlers
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

makeP2PEnvironment :: NetworkConfig -> Int -> Int -> Int -> IO P2PEnv
makeP2PEnvironment nc@NetworkConfig {..} sbound pingThreshold kademliaConcurrencyFactor = do
    nmap <- newTVarIO HM.empty
    r2pmap <- newTVarIO HM.empty
    dr2pmap <- newTVarIO HM.empty
    kb <-
        createKbucket
            (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor
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
        P2PEnv
        { _networkConfig = nc
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

data Handlers = Handlers
    { rpc :: forall m t msg. (HasP2PEnv m) =>
                                 Request t msg -> m (Response t msg)
    , kademlia :: forall m t msg. (HasP2PEnv m) =>
                                      Request t msg -> m (Response t msg)
    }

makeLensesWith classUnderscoreNoPrefixFields ''P2PEnv
