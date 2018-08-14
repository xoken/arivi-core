{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where
import           Arivi.Env
import           Arivi.P2P.Types (NetworkConfig(..))
import           Arivi.P2P.Kademlia.Types              (createKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Lens.TH
import           Data.HashMap.Strict                   as HM

data P2PEnv = P2PEnv
    { _networkConfig :: NetworkConfig
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
    }

class (T.HasKbucket m, HasStatsdClient m, HasNetworkEnv m, HasSecretKey m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getAriviTVarP2PEnv :: m NetworkConfig
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getArchivedResourceToPeerMapP2PEnv :: m (TVar ArchivedResourceToPeerMap)
    getMessageTypeMapP2PEnv :: m (MessageTypeMap m)
    getWatcherTableP2PEnv :: m (TVar WatchersTable)
    getNotifiersTableP2PEnv :: m (TVar NotifiersTable)
    getTopicHandlerMapP2PEnv :: m (TVar TopicHandlerMap)
    getMessageHashMapP2PEnv :: m (TVar MessageHashMap)
    getTransientResourceToPeerMap :: m (TVar TransientResourceToPeerMap)

makeP2PEnvironment ::
       NetworkConfig
    -> Int
    -> Int
    -> Int
    -> IO P2PEnv
makeP2PEnvironment nc@NetworkConfig{..} sbound pingThreshold kademliaConcurrencyFactor = do
    nmap <- newTVarIO HM.empty
    r2pmap <- newTVarIO HM.empty
    dr2pmap <- newTVarIO HM.empty
    kb <-
        createKbucket
            (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor
    let mtypemap = HM.empty
    watcherMap <- newTVarIO HM.empty
    notifierMap <- newTVarIO HM.empty
    topicHandleMap <- newTVarIO HM.empty
    messageMap <- newTVarIO HM.empty
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
            }

makeLensesWith classUnderscoreNoPrefixFields ''P2PEnv
