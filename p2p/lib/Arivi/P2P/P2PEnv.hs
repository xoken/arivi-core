{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds, FunctionalDependencies #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where

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

-- |Upon writing services, we might discover that topic (t) and resource (r)
-- can be the same type, and the same with rpc message (rmsg) and
-- pubsub message (pmsg), for now it has become a huge thing in itself that
-- modifying it affects a lot of other modules.
type HasP2PEnv env m r t rmsg pmsg
     = ( HasNodeEndpoint m
       , HasLogging m
       , HasPubSub env t
       , HasRpc env r rmsg
       , HasKbucket m
       , HasStatsdClient m
       , HasPRT m
       , MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasRpcGlobalHandler env m r t rmsg pmsg
       , HasPSGlobalHandler env m r t rmsg pmsg)

data NodeEndpointEnv =
    NodeEndpointEnv
        { _networkConfig :: NetworkConfig
        , tvarNodeIdPeerMap :: TVar NodeIdPeerMap
        , handlers :: Handlers
        , ariviNetworkEnv :: AriviEnv
        }

mkNodeEndpoint :: NetworkConfig -> Handlers -> AriviEnv -> IO NodeEndpointEnv
mkNodeEndpoint nc handlers ne = do
    peerMap <- newTVarIO HM.empty
    return $ NodeEndpointEnv nc peerMap handlers ne

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

class HasPRT m where
    getPeerReputationHistoryTableTVar :: m (TVar PeerReputationHistoryTable)
    getServicesReputationHashMapTVar :: m (TVar ServicesReputationHashMap)
    getP2PReputationHashMapTVar :: m (TVar P2PReputationHashMap)
    getReputedVsOtherTVar :: m (TVar Rational)
    getKClosestVsRandomTVar :: m (TVar Rational)

mkPRTEnv :: IO PRTEnv
mkPRTEnv = do
    peerReputationHashTable <- newTVarIO HM.empty
    servicesReputationHashMapTVar <- newTVarIO HM.empty
    p2pReputationHashMapTVar <- newTVarIO HM.empty
    reputedVsOtherTVar <- newTVarIO (1 % 1 :: Rational)
    kClosestVsRandomTVar <- newTVarIO (1 % 1 :: Rational)
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
