{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Arivi.P2P.SeparateEnv
    ( module Arivi.P2P.SeparateEnv
    ) where

import           Control.Concurrent.STM                (TVar)
import           Data.HashMap.Strict                   as HM
import           Arivi.Env                             (HasNetworkEnv(..), AriviEnv, HasSecretKey(..))
import           Data.ByteString.Lazy                   (ByteString)
import           Control.Monad.Reader
import           Codec.Serialise
import           GHC.Generics


type NodeId = ByteString
data PeerDetails = PeerDetails {
    peerIp :: String
}

data NetworkConfig = NetworkConfig {
    myIp :: String
}

type NodeIdPeerMap = HM.HashMap NodeId PeerDetails

data NodeEndpointEnv = NodeEndpointEnv {
      networkConfig :: NetworkConfig
    , nodeToPeerMap :: TVar NodeIdPeerMap
    , networkEnv :: AriviEnv
}

class HasNodeEndpoint m where
    getEndpointEnv :: m NodeEndpointEnv
    getNetworkConfig :: m NetworkConfig
    getPeerMap :: m (TVar NodeIdPeerMap)


newtype ResourceMap r = ResourceMap {
    getMapR :: HM.HashMap r Int
}

data RpcEnv r = RpcEnv {
    resourceMap :: TVar (ResourceMap r)
}

class HasRpc r where
    getResourceMap :: TVar (ResourceMap r)


data P2PEnv r t = P2PEnv {
      nodeEndpointEnv :: NodeEndpointEnv
    , rpcEnv          :: RpcEnv r
    , pubsubEnv       :: PubsubEnv t
}


class Resource r where
    resourceId :: r -> String

data Resources = BlockResource deriving(Eq, Ord, Show, Generic, Serialise)

newtype TopicMap t = TopicMap {
    getMapT :: HM.HashMap t Int
}

data PubsubEnv t = PubSubEnv {
    topicMap :: TVar (TopicMap t)
}

class HasPubsub t where
    getTopicMap :: TVar (TopicMap t)

class Topic t where
    topicId :: t -> String

data Topics = BlockTopic deriving(Eq, Ord, Show, Generic, Serialise)

type AppM = ReaderT (P2PEnv Resources Topics) IO

instance HasSecretKey AppM

instance HasNetworkEnv AppM where
    getEnv = networkEnv <$> getEndpointEnv

instance HasNodeEndpoint AppM where
    getEndpointEnv = asks nodeEndpointEnv
    getNetworkConfig = networkConfig <$> getEndpointEnv
    getPeerMap = nodeToPeerMap <$> getEndpointEnv
