{-# LANGUAGE MultiParamTypeClasses #-}
<<<<<<< HEAD
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
=======
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds, FunctionalDependencies #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where

<<<<<<< HEAD
import           Arivi.Env
import           Arivi.P2P.Types (NetworkConfig(..), RpcPayload(..), Request(..), Response(..), PubSub(..))
import           Arivi.P2P.Kademlia.Types              (HasKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Env
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.PRT.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd

import           Codec.Serialise
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Concurrent.STM                (TVar, newTVarIO)
-- import           Control.Lens.TH
import           Data.ByteString.Lazy                  (ByteString)
import           Data.HashMap.Strict                   as HM
import           Data.Hashable
import           Data.Ratio                            (Rational, (%))
=======
import Arivi.Env
import Arivi.P2P.Kademlia.Types (HasKbucket)
import qualified Arivi.P2P.Kademlia.Types as T
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.PRT.Types
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types (NetworkConfig(..), PubSub(..), Request(..), Response(..), RpcPayload(..))
import Arivi.Utils.Logging
import Arivi.Utils.Statsd
import Codec.Serialise
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict as HM
import Data.Ratio (Rational, (%))
>>>>>>> breaking out arivi-core from arivi

-- |Upon writing services, we might discover that topic (t) and resource (r)
-- can be the same type, and the same with rpc message (rmsg) and
-- pubsub message (pmsg), for now it has become a huge thing in itself that
-- modifying it affects a lot of other modules.
type HasP2PEnv env m r t rmsg pmsg
     = ( HasNodeEndpoint m
       , HasLogging m
<<<<<<< HEAD
       , HasPubSub env t pmsg
       , HasRpc m r rmsg
=======
       , HasPubSub env t
       , HasRpc env r rmsg
>>>>>>> breaking out arivi-core from arivi
       , HasKbucket m
       , HasStatsdClient m
       , HasPRT m
       , MonadReader env m
       , HasNetworkConfig env NetworkConfig
<<<<<<< HEAD
       )

data NodeEndpointEnv = NodeEndpointEnv {
      _networkConfig :: NetworkConfig
    , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
    , handlers          :: Handlers
    , ariviNetworkEnv :: AriviEnv
}
=======
       , HasRpcGlobalHandler env m r t rmsg pmsg
       , HasPSGlobalHandler env m r t rmsg pmsg)

data NodeEndpointEnv =
    NodeEndpointEnv
        { _networkConfig :: NetworkConfig
        , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
        , handlers :: Handlers
        , ariviNetworkEnv :: AriviEnv
        }
>>>>>>> breaking out arivi-core from arivi

mkNodeEndpoint :: NetworkConfig -> Handlers -> AriviEnv -> IO NodeEndpointEnv
mkNodeEndpoint nc handlers ne = do
    peerMap <- newTVarIO HM.empty
    return $ NodeEndpointEnv nc peerMap handlers ne

<<<<<<< HEAD

data RpcEnv r m = RpcEnv {
      tvarArchivedResourceToPeerMap :: TVar (ArchivedResourceToPeerMap r m)
    , tvarDynamicResourceToPeerMap :: TVar (TransientResourceToPeerMap r m)
}

mkRpcEnv :: IO (RpcEnv r m)
mkRpcEnv =
    RpcEnv <$> newTVarIO (ArchivedResourceToPeerMap HM.empty)
           <*> newTVarIO (TransientResourceToPeerMap HM.empty)

data KademliaEnv = KademliaEnv {
    kbucket :: T.Kbucket Int [T.Peer]
}

mkKademlia :: NetworkConfig -> Int -> Int -> Int -> IO KademliaEnv
mkKademlia NetworkConfig{..} sbound pingThreshold kademliaConcurrencyFactor =
    KademliaEnv <$>
        T.createKbucket
            (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor


data P2PEnv r t rmsg pmsg = P2PEnv {
      nodeEndpointEnv :: NodeEndpointEnv
    , rpcEnv :: RpcEnv r rmsg
    , psEnv :: PubSubEnv t pmsg
    , kademliaEnv :: KademliaEnv
    , statsdClient :: StatsdClient
    , prtEnv       :: PRTEnv
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

data PRTEnv = PRTEnv {
    tvPeerReputationHashTable     :: TVar PeerReputationHistoryTable
  , tvServicesReputationHashMap   :: TVar ServicesReputationHashMap
  , tvP2PReputationHashMap        :: TVar P2PReputationHashMap
  , tvReputedVsOther              :: TVar Rational
  , tvKClosestVsRandom            :: TVar Rational
}
=======
data KademliaEnv =
    KademliaEnv
        { kbucket :: T.Kbucket Int [T.Peer]
        }

mkKademlia :: NetworkConfig -> Int -> Int -> Int -> IO KademliaEnv
mkKademlia NetworkConfig {..} sbound pingThreshold kademliaConcurrencyFactor -- hopBound =
 =
    KademliaEnv <$>
    T.createKbucket
        (T.Peer _nodeId (T.NodeEndPoint _ip _tcpPort _udpPort))
        sbound
        pingThreshold
        kademliaConcurrencyFactor
            -- hopBound

data P2PEnv m r t rmsg pmsg =
    P2PEnv
        { nodeEndpointEnv :: NodeEndpointEnv
        , rEnv :: RpcEnv r rmsg
        , psEnv :: PubSubEnv t
        , kademliaEnv :: KademliaEnv
        , statsdClient :: StatsdClient
        , prtEnv :: PRTEnv
        , rHandler :: rmsg -> m (Maybe rmsg)
        , psHandler :: t -> pmsg -> (m Status)
        }

class HasPSGlobalHandler env m r t rmsg pmsg | env -> m r t rmsg where
    psGlobalHandler :: env -> (t -> pmsg -> m Status)

instance HasPSGlobalHandler (P2PEnv m r t rmsg pmsg) m r t rmsg pmsg where
    psGlobalHandler = psHandler

class HasRpcGlobalHandler env m r t rmsg pmsg | env -> m r t pmsg where
    rpcGlobalHandler :: env -> (rmsg -> m (Maybe rmsg))

instance HasRpcGlobalHandler (P2PEnv m r t rmsg pmsg) m r t rmsg pmsg where
    rpcGlobalHandler = rHandler

class (HasSecretKey m) =>
      HasNodeEndpoint m
    where
    getEndpointEnv :: m NodeEndpointEnv
    getNetworkConfig :: m NetworkConfig
    getHandlers :: m Handlers
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)

data PRTEnv =
    PRTEnv
        { tvPeerReputationHashTable :: TVar PeerReputationHistoryTable
        , tvServicesReputationHashMap :: TVar ServicesReputationHashMap
        , tvP2PReputationHashMap :: TVar P2PReputationHashMap
        , tvReputedVsOther :: TVar Rational
        , tvKClosestVsRandom :: TVar Rational
        }
>>>>>>> breaking out arivi-core from arivi

class HasPRT m where
    getPeerReputationHistoryTableTVar :: m (TVar PeerReputationHistoryTable)
    getServicesReputationHashMapTVar :: m (TVar ServicesReputationHashMap)
    getP2PReputationHashMapTVar :: m (TVar P2PReputationHashMap)
    getReputedVsOtherTVar :: m (TVar Rational)
    getKClosestVsRandomTVar :: m (TVar Rational)

<<<<<<< HEAD
mkPRTEnv :: IO PRTEnv 
=======
mkPRTEnv :: IO PRTEnv
>>>>>>> breaking out arivi-core from arivi
mkPRTEnv = do
    peerReputationHashTable <- newTVarIO HM.empty
    servicesReputationHashMapTVar <- newTVarIO HM.empty
    p2pReputationHashMapTVar <- newTVarIO HM.empty
    reputedVsOtherTVar <- newTVarIO (1 % 1 :: Rational)
    kClosestVsRandomTVar <- newTVarIO (1 % 1 :: Rational)
<<<<<<< HEAD
    return (PRTEnv peerReputationHashTable servicesReputationHashMapTVar p2pReputationHashMapTVar reputedVsOtherTVar kClosestVsRandomTVar) 


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
      rpc :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => Request 'Rpc (RpcPayload r msg) -> m (Response 'Rpc (RpcPayload r msg))
    , kademlia :: forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg) => Request 'Kademlia T.PayLoad -> m (Response 'Kademlia T.PayLoad)
    , option :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => m (Response 'Option (Supported [r]))
    , pubsub :: forall env m t msg. (MonadReader env m, HasPubSub env t msg, MonadIO m) => NodeId -> PubSub -> ByteString -> m ByteString
}
=======
    return
        (PRTEnv
             peerReputationHashTable
             servicesReputationHashMapTVar
             p2pReputationHashMapTVar
             reputedVsOtherTVar
             kClosestVsRandomTVar)

data Handlers =
    Handlers
        { rpc :: forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg, MonadIO m) =>
                                                 Request 'Rpc (RpcPayload r rmsg) -> m (Response 'Rpc (RpcPayload r rmsg))
        , kademlia :: forall env m r t rmsg pmsg. (Serialise pmsg, Show t) =>
                                                      (HasP2PEnv env m r t rmsg pmsg) =>
                                                          Request 'Kademlia T.PayLoad -> m (Response 'Kademlia T.PayLoad)
        , option :: forall env m r msg. (MonadReader env m, HasNodeEndpoint m, HasRpc env r msg, MonadIO m) =>
                                            m (Response 'Option (Supported [r]))
        , pubsub :: forall env m r t rmsg pmsg. (Serialise pmsg, Show t) =>
                                                    (HasP2PEnv env m r t rmsg pmsg, MonadIO m) =>
                                                        NodeId -> PubSub -> ByteString -> m ByteString
        }

instance HasNetworkConfig (P2PEnv m r t rmsg pmsg) NetworkConfig where
    networkConfig f p2p =
        fmap
            (\nc -> p2p {nodeEndpointEnv = (nodeEndpointEnv p2p) {Arivi.P2P.P2PEnv._networkConfig = nc}})
            (f ((Arivi.P2P.P2PEnv._networkConfig . nodeEndpointEnv) p2p))

instance HasTopics (P2PEnv m r t rmsg pmsg) t where
    topics = pubSubTopics . psEnv

instance HasSubscribers (P2PEnv m r t rmsg pmsg) t where
    subscribers = pubSubSubscribers . psEnv

instance HasNotifiers (P2PEnv m r t rmsg pmsg) t where
    notifiers = pubSubNotifiers . psEnv

instance HasPubSubEnv (P2PEnv m r t rmsg pmsg) t where
    pubSubEnv = psEnv

instance HasRpcEnv (P2PEnv m r t rmsg pmsg) r rmsg where
    rpcEnv = rEnv
>>>>>>> breaking out arivi-core from arivi
