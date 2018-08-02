{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
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

data Rpc r msg  = RpcRequest r msg
                | RpcResponse r msg
                deriving (Eq, Ord, Show, Generic)

instance (Serialise r, Serialise msg) => Serialise (Rpc r msg)

data Response
    = Error
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise Response

data PubSub t msg = Subscribe t UTCTime
                  | Notify t msg
                  | Publish t msg
                  | PubSubResponse t Response UTCTime
                  deriving (Eq, Ord, Show, Generic)

instance (Serialise t, Serialise msg) => Serialise (PubSub t msg)

data Supported r t = Request
                   | Response r t
                   deriving (Eq, Ord, Show, Generic)

instance (Serialise r, Serialise t) => Serialise (Supported r t)

getResource :: (Message msg) => k -> Map k [NodeId] -> -> Map NodeId Peer -> msg -> IO msg
getResource = do
  return ()

getResourceFromNodeId :: (Message msg) -> NodeId -> Map NodeId Peer -> msg -> IO msg
getResourceFromNodeId = do
  return ()

makeLensesWith classUnderscoreNoPrefixFields ''NetworkConfig
