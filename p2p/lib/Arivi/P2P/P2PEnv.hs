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
{-# LANGUAGE DataKinds, ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where
import           Arivi.Env
import           Arivi.P2P.Types -- (NetworkConfig(..), Request ,Response, RpcPayload(..))
import           Arivi.P2P.Kademlia.Types              (createKbucket, HasKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Statsd
import           Codec.Serialise
import           Control.Monad.IO.Class
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Lens.TH
import           Data.HashMap.Strict                   as HM
import           Data.Hashable
import           Data.Proxy

-- data P2PEnv = P2PEnv
--     { _networkConfig :: NetworkConfig
--     , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
--     , tvarArchivedResourceToPeerMap :: TVar ArchivedResourceToPeerMap
--     , kbucket :: T.Kbucket Int [T.Peer]
--     , statsdClient :: StatsdClient
--     , tvarMessageTypeMap :: Handlers
--     , tvarWatchersTable :: TVar WatchersTable
--     , tvarNotifiersTable :: TVar NotifiersTable
--     , tvarTopicHandlerMap :: TVar TopicHandlerMap
--     , tvarMessageHashMap :: TVar MessageHashMap
--     , ariviNetworkEnv :: AriviEnv
--     , tvarDynamicResourceToPeerMap :: TVar TransientResourceToPeerMap
--     }

data NodeEndPointEnv = NodeEndPointEnv {
      _networkConfig :: NetworkConfig
    , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
    , tvarMessageTypeMap :: Handlers
    , ariviNetworkEnv :: AriviEnv
}

mkNodeEndpoint :: NetworkConfig -> TVar NodeIdPeerMap -> Handlers -> AriviEnv -> NodeEndPointEnv
mkNodeEndpoint = NodeEndPointEnv


data RpcEnv r = RpcEnv {
      tvarArchivedResourceToPeerMap :: TVar (ArchivedResourceToPeerMap r)
    , tvarDynamicResourceToPeerMap :: TVar (TransientResourceToPeerMap r)
}


-- data PubSubEnv = PubSubEnv {
--       tvarWatchersTable :: TVar WatchersTable
--     , tvarNotifiersTable :: TVar NotifiersTable
--     , tvarTopicHandlerMap :: TVar TopicHandlerMap
--     , tvarMessageHashMap :: TVar MessageHashMap
-- }

data KademliaEnv = KademliaEnv {
    kbucket :: T.Kbucket Int [T.Peer]
}

data P2PEnv r = P2PEnv {
      nodeEndpointEnv :: NodeEndPointEnv
    , rpcEnv :: RpcEnv r
    , kademliaEnv :: KademliaEnv
    -- , pubSubEnv :: PubSubEnv
    , statsdClient :: StatsdClient
}


class (HasSecretKey m) => HasNodeEndpoint m where
    getEndpointEnv :: m NodeEndPointEnv
    getNetworkConfig :: m NetworkConfig
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getMessageTypeMapP2PEnv :: m Handlers

-- The actual hashmap key of type String would depend on the MessageSubType
class HasRpc m (r :: MessageSubType) where
    getArchivedResourceToPeerMapP2PEnv :: Proxy r -> m (TVar (ArchivedResourceToPeerMap String))
    getTransientResourceToPeerMap :: Proxy r -> m (TVar (TransientResourceToPeerMap String))

-- class HasPubSub where
--     getWatcherTableP2PEnv :: TVar WatchersTable
--     getNotifiersTableP2PEnv :: TVar NotifiersTable
--     getTopicHandlerMapP2PEnv :: TVar TopicHandlerMap
--     getMessageHashMapP2PEnv :: TVar MessageHashMap


-- class (T.HasKbucket m, HasStatsdClient m, HasNetworkEnv m, HasSecretKey m) =>
--       HasP2PEnv m
--     where
--     getP2PEnv :: m P2PEnv
--     getAriviTVarP2PEnv :: m NetworkConfig
--     getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
--     getArchivedResourceToPeerMapP2PEnv :: m (TVar ArchivedResourceToPeerMap)
--     getMessageTypeMapP2PEnv :: m Handlers
--     getWatcherTableP2PEnv :: m (TVar WatchersTable)
--     getNotifiersTableP2PEnv :: m (TVar NotifiersTable)
--     getTopicHandlerMapP2PEnv :: m (TVar TopicHandlerMap)
--     getMessageHashMapP2PEnv :: m (TVar MessageHashMap)
--     getTransientResourceToPeerMap :: m (TVar TransientResourceToPeerMap)


makeP2PEnvironment ::
       NetworkConfig
    -> Int
    -> Int
    -> Int
    -> IO (P2PEnv r)
makeP2PEnvironment nc@NetworkConfig{..} sbound pingThreshold kademliaConcurrencyFactor = do
    r2pmap <- newTVarIO (ArchivedResourceToPeerMap HM.empty)
    dr2pmap <- newTVarIO (TransientResourceToPeerMap HM.empty)
    let rpcEnv = RpcEnv r2pmap dr2pmap
    kb <-
        createKbucket
            (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor
    let kademliaEnv = KademliaEnv kb
    -- watcherMap <- newTVarIO HM.empty
    -- notifierMap <- newTVarIO HM.empty
    -- topicHandleMap <- newTVarIO HM.empty
    -- messageMap <- newTVarIO HM.empty
    -- let pubSubEnv = PubSubEnv
    return P2PEnv {
        rpcEnv = rpcEnv
      , kademliaEnv = kademliaEnv
      -- , pubSubEnv :: PubSubEnv
    }
        -- P2PEnv
        --     { _networkConfig = nc
        --     , tvarNodeIdPeerMap = nmap
        --     , tvarArchivedResourceToPeerMap = r2pmap
        --     , tvarWatchersTable = watcherMap
        --     , tvarNotifiersTable = notifierMap
        --     , tvarTopicHandlerMap = topicHandleMap
        --     , tvarMessageHashMap = messageMap
        --     , tvarDynamicResourceToPeerMap = dr2pmap
        --     , kbucket = kb
        --     }

data Handlers = Handlers
  { rpc :: forall m r t msg. (HasRpc m r, HasNodeEndpoint m, MonadIO m)
        => Request t r msg -> m (Response t r msg)
  , kademlia :: forall m t msg. (HasKbucket m, Serialise msg)
        => Request t 'None msg -> m (Response t 'None msg)
  }


makeLensesWith classUnderscoreNoPrefixFields ''P2PEnv
