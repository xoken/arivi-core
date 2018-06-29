{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    ) where

import Arivi.Env
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Arivi.Utils.Logging

import qualified Arivi.P2P.Kademlia.Types as T
import Arivi.Utils.Statsd
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Concurrent.STM.TQueue
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.HashMap.Strict as HM
import qualified STMContainers.Map as H

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
    }

type P2Papp = ReaderT P2PEnv (LoggingT IO)

instance HasNetworkEnv P2Papp where
    getEnv = asks ariviNetworkEnv

instance HasSecretKey P2Papp

instance HasLogging P2Papp

class ( T.HasKbucket m
      , HasStatsdClient m
      , HasNetworkEnv m
      , HasLogging m
      , HasSecretKey m
      ) =>
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

instance T.HasKbucket P2Papp where
    getKb = asks kbucket

instance HasStatsdClient P2Papp where
    getStatsdClient = asks statsdClient

instance HasP2PEnv P2Papp where
    getP2PEnv = ask
    getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getkademTQueueP2PEnv = tqueueKadem <$> getP2PEnv
    getrpcTQueueP2PEnv = tqueueRPC <$> getP2PEnv
    getpubsubTQueueP2PEnv = tqueuePubSub <$> getP2PEnv
    getoptionTQueueP2PEnv = tqueueOption <$> getP2PEnv
    getResourceToPeerMapP2PEnv = tvarResourceToPeerMap <$> getP2PEnv
    getMessageTypeMapP2PEnv = tvarMessageTypeMap <$> getP2PEnv
    getWatcherTableP2PEnv = tvarWatchersTable <$> getP2PEnv
    getNotifiersTableP2PEnv = tvarNotifiersTable <$> getP2PEnv
    getTopicHandlerMapP2PEnv = tvarTopicHandlerMap <$> getP2PEnv
    getMessageHashMapP2PEnv = tvarMessageHashMap <$> getP2PEnv

runP2Papp :: P2PEnv -> P2Papp a -> LoggingT IO a
runP2Papp = flip runReaderT

makeP2PEnvironment :: IO P2PEnv
makeP2PEnvironment = do
    nmap <- newTVarIO HM.empty
    kqueue <- newTQueueIO
    rqueue <- newTQueueIO
    pqueue <- newTQueueIO
    oqueue <- newTQueueIO
    r2pmap <- newTVarIO HM.empty
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
            }
