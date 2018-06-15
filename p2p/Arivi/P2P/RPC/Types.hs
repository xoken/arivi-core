{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.RPC.Types
    ( ServiceId
    , ResourceList
    , ResourceToPeerMap
    , ResourceId
    , MessageTypeRPC(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (Peer (..))
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Data.HashMap.Strict                   as HM
import           GHC.Generics                          (Generic)

type NodeId = String

type IP = String

type Port = Int

type ResourceId = String

type ServiceId = String

type ResourceList = [ResourceId]

type ResourceToPeerMap = HM.HashMap ResourceId (ServiceId, TQueue Peer)

data MessageTypeRPC
    = Options { to   :: NodeId
              , from :: NodeId }
    | Support { to                 :: NodeId
              , from               :: NodeId
              , supportedResources :: [ResourceId] }
    deriving (Eq, Ord, Show, Generic)

instance Serialise MessageTypeRPC
