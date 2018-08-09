{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor,
  DeriveTraversable, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

-- |
-- Module      :  Arivi.P2P.Types
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different data types that are used in the P2P layer
--
module Arivi.P2P.Types where

import           Arivi.Network.Types (NodeId)

import           Codec.Serialise
import           GHC.Generics        (Generic)
import           Network.Socket      (HostName, PortNumber)
import           Control.Lens.TH
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Proxy

type Map = HashMap

data NetworkConfig = NetworkConfig
    { _nodeId  :: NodeId
    , _ip      :: HostName
    , _udpPort :: PortNumber
    , _tcpPort :: PortNumber
    } deriving (Eq, Ord, Show, Generic)

data RpcRequest msg r  = RpcRequest msg r  deriving (Eq, Ord, Show, Generic)
data RpcResponse msg r =  RpcResponse msg r deriving (Eq, Ord, Show, Generic)

instance (Serialise r, Serialise msg) => Serialise (RpcRequest r msg)
instance (Serialise r, Serialise msg) => Serialise (RpcResponse r msg)

data MessageType = Kademlia
                 | Options
                 | Rpc
                 | PubSub
                 deriving (Eq, Show, Ord, Generic, Serialise, Hashable)

{-
data Request (i :: MessageType)  where
  RpcRequestG :: (Serialise m, Serialise r) => RpcRequest m r -> Request 'Rpc
  OptionsRequestG :: SRequest -> Request 'Options

data Response (i :: MessageType) where
  RpcResponseG :: RpcResponse m r -> Response 'Rpc
  OptionsResponseG :: SResponse r -> Response 'Options

data ResponseCode
    = Error
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

data PubSub msg t = Subscribe UTCTime t
                  | Notify msg t
                  | Publish msg t
                  | PubSubResponse ResponseCode UTCTime t
                  deriving (Eq, Ord, Show, Generic)

deriving instance Functor (PubSub msg)
deriving instance Foldable (PubSub msg)
deriving instance Traversable (PubSub msg)

instance (Serialise t, Serialise msg) => Serialise (PubSub t msg)

data SRequest = SRequest deriving (Eq, Ord, Show, Generic, Serialise)
data SResponse r = SResponse r deriving (Eq, Ord, Show, Generic, Serialise)

newtype Handler i o m = Handler { runHandler :: i -> m o }
-}

data Kademlia
data Rpc

deriving instance Generic Kademlia
deriving instance Serialise Kademlia

newtype Request i msg  = Request msg deriving (Eq, Ord, Show, Generic, Serialise)
newtype Response i msg = Response msg deriving (Eq, Ord, Show, Generic, Serialise)

class Msg i where
  msgType :: Proxy i -> MessageType

instance Msg i => Msg (Request i msg) where
  msgType _ = msgType (Proxy :: Proxy i)


instance Msg i => Msg (Response i msg) where
  msgType _ = msgType (Proxy :: Proxy i)

instance Msg Rpc where
  msgType _ = Rpc

instance Msg Kademlia where
  msgType _ = Kademlia

makeLensesWith classUnderscoreNoPrefixFields ''NetworkConfig
