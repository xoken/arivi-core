{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types     #-}

module Arivi.P2P.RPC.Types
    ( ArchivedResourceToPeerMap(..)
    , NodeId
    , ResourceId
    , ServiceMessage
    , ResourceHandler(..)
    , TransientResourceToPeerMap(..)
    , ResourceType(..)
    , Options(..)
    , Supported(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId, P2PUUID)
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import           Data.Hashable
import           Data.Hashable
import           Data.HashMap.Strict                   as HM
import           GHC.Generics                          (Generic)

import           Arivi.P2P.Types                       (Resource,
                                                        RpcPayload (..))

type ServiceMessage = Lazy.ByteString
type ResourceId = String


newtype ArchivedResourceToPeerMap = ArchivedResourceToPeerMap {
    getArchivedMap :: forall r . (Resource r) =>  HM.HashMap r  (ResourceHandler, TVar [NodeId])
}

newtype ResourceHandler = ResourceHandler (forall r m . RpcPayload r m -> RpcPayload r m)

data Options = Options deriving (Eq, Ord, Show, Generic, Serialise)

data Supported r = Supported r deriving(Eq, Ord, Generic, Serialise, Hashable)


newtype TransientResourceToPeerMap = TransientResourceToPeerMap {
    getTransientMap :: forall r . (Resource r) =>  HM.HashMap r (ResourceHandler, TVar [NodeId])
}

data ResourceType
    = Archived
    | Transient
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResourceType
