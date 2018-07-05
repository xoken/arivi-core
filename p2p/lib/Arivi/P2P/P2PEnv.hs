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
import           Arivi.P2P.Kademlia.Kbucket            (createKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.Types
import           Arivi.Utils.Statsd
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Concurrent.STM.TQueue

import           Data.HashMap.Strict                   as HM
import           Network.Socket                        (PortNumber)

data P2PEnv = P2PEnv
    { tvarAriviP2PInstance :: TVar AriviP2PInstance
    , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
    , tqueueKadem :: TQueue MessageInfo
    , tqueueRPC :: TQueue MessageInfo
    , tqueuePubSub :: TQueue MessageInfo
    , tqueueOption :: TQueue MessageInfo
    , tvarResourceToPeerMap :: TVar ResourceToPeerMap
    , kbucket :: T.Kbucket Int [T.Peer]
    , statsdClient :: StatsdClient
    , tvarMessageTypeMap :: forall m. (HasP2PEnv m) =>
                                          (MessageTypeMap m)
    , tvarWatchersTable :: TVar WatchersTable
    , tvarNotifiersTable :: TVar NotifiersTable
    , tvarTopicHandlerMap :: TVar TopicHandlerMap
    , tvarMessageHashMap :: TVar MessageHashMap
    , ariviNetworkEnv :: AriviEnv
    , tvarDynamicResourceToPeerMap :: TVar DynamicResourceToPeerMap
    , kademliaConcurrencyFactor :: Int
    }

class (T.HasKbucket m, HasStatsdClient m, HasNetworkEnv m, HasSecretKey m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getAriviTVarP2PEnv :: m (TVar AriviP2PInstance)
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getkademTQueueP2PEnv :: m (TQueue MessageInfo)
    getrpcTQueueP2PEnv :: m (TQueue MessageInfo)
    getpubsubTQueueP2PEnv :: m (TQueue MessageInfo)
    getoptionTQueueP2PEnv :: m (TQueue MessageInfo)
    getResourceToPeerMapP2PEnv :: m (TVar ResourceToPeerMap)
    getMessageTypeMapP2PEnv :: m (MessageTypeMap m)
    getWatcherTableP2PEnv :: m (TVar WatchersTable)
    getNotifiersTableP2PEnv :: m (TVar NotifiersTable)
    getTopicHandlerMapP2PEnv :: m (TVar TopicHandlerMap)
    getMessageHashMapP2PEnv :: m (TVar MessageHashMap)
    getDynamicResourceToPeerMap :: m (TVar DynamicResourceToPeerMap)
    getKademliaConcurrencyFactor :: m Int

makeP2PEnvironment ::
       String -> T.NodeId -> PortNumber -> PortNumber -> Int -> IO P2PEnv
makeP2PEnvironment nIp nId tPort uPort alpha = do
    nmap <- newTVarIO HM.empty
    kqueue <- newTQueueIO
    rqueue <- newTQueueIO
    pqueue <- newTQueueIO
    oqueue <- newTQueueIO
    r2pmap <- newTVarIO HM.empty
    dr2pmap <- newTVarIO HM.empty
    kb <- createKbucket (T.Peer (nId, T.NodeEndPoint nIp tPort uPort))
    let mtypemap = HM.empty
    watcherMap <- newTVarIO HM.empty
    notifierMap <- newTVarIO HM.empty
    topicHandleMap <- newTVarIO HM.empty
    messageMap <- newTVarIO HM.empty
    return
        P2PEnv
            { tvarNodeIdPeerMap = nmap
            , tqueueKadem = kqueue
            , tqueueRPC = rqueue
            , tqueuePubSub = pqueue
            , tqueueOption = oqueue
            , tvarResourceToPeerMap = r2pmap
            , tvarMessageTypeMap = mtypemap
            , tvarWatchersTable = watcherMap
            , tvarNotifiersTable = notifierMap
            , tvarTopicHandlerMap = topicHandleMap
            , tvarMessageHashMap = messageMap
            , tvarDynamicResourceToPeerMap = dr2pmap
            , kbucket = kb
            , kademliaConcurrencyFactor = alpha
            }
