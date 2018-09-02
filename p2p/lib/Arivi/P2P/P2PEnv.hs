{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds, ExistentialQuantification, ConstraintKinds  #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where
import           Arivi.Env
import           Arivi.P2P.Types (NetworkConfig(..), RpcPayload(..))
import           Arivi.P2P.Kademlia.Types              (createKbucket, HasKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Codec.Serialise
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Lens.TH
import           Data.HashMap.Strict                   as HM
import           Data.Hashable

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


type HasP2PEnv env m r msg
     = ( HasNodeEndpoint m
       , HasLogging m
       , HasRpc m r msg
       , HasKbucket m
       , HasStatsdClient m
       , MonadReader env m
       , HasNetworkConfig env NetworkConfig
       )

data NodeEndpointEnv = NodeEndpointEnv {
      _networkConfig :: NetworkConfig
    , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
    , handlers          :: Handlers
    , ariviNetworkEnv :: AriviEnv
}

mkNodeEndpoint :: NetworkConfig -> Handlers -> AriviEnv -> IO NodeEndpointEnv
mkNodeEndpoint nc handlers ne = do
    peerMap <- newTVarIO HM.empty
    return $ NodeEndpointEnv nc peerMap handlers ne


data RpcEnv r m = RpcEnv {
      tvarArchivedResourceToPeerMap :: TVar (ArchivedResourceToPeerMap r m)
    , tvarDynamicResourceToPeerMap :: TVar (TransientResourceToPeerMap r m)
}

mkRpcEnv ::  IO (RpcEnv r m)
mkRpcEnv = do
    archived <- newTVarIO (ArchivedResourceToPeerMap HM.empty)
    transient <- newTVarIO (TransientResourceToPeerMap HM.empty)
    return $ RpcEnv archived transient
-- data PubSubEnv = PubSubEnv {
--       tvarWatchersTable :: TVar WatchersTable
--     , tvarNotifiersTable :: TVar NotifiersTable
--     , tvarTopicHandlerMap :: TVar TopicHandlerMap
--     , tvarMessageHashMap :: TVar MessageHashMap
-- }

data KademliaEnv = KademliaEnv {
    kbucket :: T.Kbucket Int [T.Peer]
}

data P2PEnv r msg = P2PEnv {
      nodeEndpointEnv :: NodeEndpointEnv
    , rpcEnv :: RpcEnv r msg
    , kademliaEnv :: KademliaEnv
    , statsdClient :: StatsdClient
}

class (HasSecretKey m) => HasNodeEndpoint m where
    getEndpointEnv :: m NodeEndpointEnv
    getNetworkConfig :: m NetworkConfig
    getHandlers      :: m Handlers
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)

type HasRpc m r msg
     = ( HasArchivedResourcers m r msg
       , HasTransientResourcers m r msg
       , Serialise r
       , Serialise msg
       , Hashable r
       , Eq r
       )

class HasArchivedResourcers m r msg | m -> r msg where
  archived :: m (TVar (ArchivedResourceToPeerMap r msg))

class HasTransientResourcers m r msg | m -> r msg where
  transient :: m (TVar (TransientResourceToPeerMap r msg))

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


-- makeP2PEnvironment ::
--        NetworkConfig
--     -> Int
--     -> Int
--     -> Int
--     -> IO (P2PEnv r)
-- makeP2PEnvironment nc@NetworkConfig{..} sbound pingThreshold kademliaConcurrencyFactor = do
--     r2pmap <- newTVarIO (ArchivedResourceToPeerMap HM.empty)
--     dr2pmap <- newTVarIO (TransientResourceToPeerMap HM.empty)
--     let rpcEnv = RpcEnv r2pmap dr2pmap
--     kb <-
--         createKbucket
--             (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
--             sbound
--             pingThreshold
--             kademliaConcurrencyFactor
--     let kademliaEnv = KademliaEnv kb
--     -- watcherMap <- newTVarIO HM.empty
--     -- notifierMap <- newTVarIO HM.empty
--     -- topicHandleMap <- newTVarIO HM.empty
--     -- messageMap <- newTVarIO HM.empty
--     -- let pubSubEnv = PubSubEnv 
--     return P2PEnv {
--         rpcEnv = rpcEnv
--       , kademliaEnv = kademliaEnv
--       -- , pubSubEnv :: PubSubEnv
--     } 
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

-- data Handlers = Handlers
--   { rpc :: forall m r t msg. (HasRpc m r, HasNodeEndpoint m, Eq r, Hashable r, Serialise r, MonadIO m) => Request t msg -> m (Response t msg)
--   , kademlia :: forall m t msg. (HasKbucket m, Serialise msg) => Request t msg -> m (Response t msg)
--   }

data Handlers = Handlers {
      rpc :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => RpcPayload r msg -> m (RpcPayload r msg)
    , kademlia :: forall env m r msg. (HasP2PEnv env m r msg) => T.PayLoad -> m T.PayLoad
    , option :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => m (Supported [r])
}

makeLensesWith classUnderscoreNoPrefixFields ''P2PEnv
