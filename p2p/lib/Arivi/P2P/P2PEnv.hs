{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    , HasStatsdClient(..)
    , T.HasKbucket(..)
    ) where

import           Arivi.Env
import           Arivi.P2P.Types (NetworkConfig(..), RpcPayload(..), Request(..), Response(..), PubSub(..))
import           Arivi.P2P.Kademlia.Types              (HasKbucket)
import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Class
import           Arivi.P2P.PubSub.Env
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd

import           Codec.Serialise
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Data.ByteString.Lazy                  (ByteString)
import           Data.HashMap.Strict                   as HM
import           Data.Hashable

-- |Upon writing services, we might discover that topic (t) and resource (r)
-- can be the same type, and the same with rpc message (rmsg) and
-- pubsub message (pmsg), for now it has become a huge thing in itself that
-- modifying it affects a lot of other modules.
type HasP2PEnv env m r t rmsg pmsg
     = ( HasNodeEndpoint m
       , HasLogging m
       , HasPubSub env t pmsg
       , HasRpc m r rmsg
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


data Handlers = Handlers {
      rpc :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => Request 'Rpc (RpcPayload r msg) -> m (Response 'Rpc (RpcPayload r msg))
    , kademlia :: forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg) => Request 'Kademlia T.PayLoad -> m (Response 'Kademlia T.PayLoad)
    , option :: forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m) => m (Response 'Option (Supported [r]))
    , pubsub :: forall env m t msg. (MonadReader env m, HasPubSub env t msg, MonadIO m) => NodeId -> PubSub -> ByteString -> m ByteString
}
