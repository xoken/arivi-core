{-# LANGUAGE TemplateHaskell #-}
{-# LANgUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor, DeriveTraversable, DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

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
module Arivi.P2P.Types
    ( NetworkConfig(..)
    ) where

import           Arivi.Network.Types (NodeId)

import           Codec.Serialise
import           Data.Time
import           GHC.Generics        (Generic)
import           Network.Socket      (HostName, PortNumber)
import           Control.Lens.TH
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

type Map = HashMap

data NetworkConfig = NetworkConfig
    { _nodeId  :: NodeId
    , _ip      :: HostName
    , _udpPort :: PortNumber
    , _tcpPort :: PortNumber
    } deriving (Eq, Ord, Show, Generic)

data Rpc msg r  = RpcRequest msg r
                | RpcResponse msg r
                deriving (Eq, Ord, Show, Generic)

deriving instance Functor (Rpc msg)
deriving instance Foldable (Rpc msg)
deriving instance Traversable (Rpc msg)

instance (Serialise r, Serialise msg) => Serialise (Rpc r msg)

data Response
    = Error
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise Response

data PubSub msg t = Subscribe UTCTime t
                  | Notify msg t
                  | Publish msg t
                  | PubSubResponse Response UTCTime t
                  deriving (Eq, Ord, Show, Generic)

deriving instance Functor (PubSub msg)
deriving instance Foldable (PubSub msg)
deriving instance Traversable (PubSub msg)

instance (Serialise t, Serialise msg) => Serialise (PubSub t msg)

data Supported r t = Request
                   | Response r t
                   deriving (Eq, Ord, Show, Generic)

instance (Serialise r, Serialise t) => Serialise (Supported r t)

newtype Handler msg m = Handler { runHandler :: (Monad m) => msg -> m msg }

makeLensesWith classUnderscoreNoPrefixFields ''NetworkConfig
