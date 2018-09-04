{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}

module Arivi.P2P.RPC.Types
    ( ArchivedResourceToPeerMap(..)
    , NodeId
    , ResourceId
    , ServiceMessage
    , ResourceHandler(..)
    , TransientResourceToPeerMap(..)
    -- , ResourceType(..)
    , Options(..)
    , Supported(..)
    , ArchivedOrTransient(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import           Data.HashMap.Strict                   as HM
import           Data.Hashable
import           GHC.Generics                          (Generic)

import           Arivi.P2P.Types (RpcPayload(..))

type ServiceMessage = Lazy.ByteString
type ResourceId = String


newtype ArchivedResourceToPeerMap r m = ArchivedResourceToPeerMap {
    getArchivedMap :: HM.HashMap r (ResourceHandler r m, TVar [NodeId])
}

newtype ResourceHandler r msg = ResourceHandler (RpcPayload r msg -> RpcPayload r msg)

data Options r = Options deriving (Eq, Ord, Show, Generic, Serialise)

data Supported r = Supported r deriving(Eq, Ord, Generic, Serialise, Hashable)


newtype TransientResourceToPeerMap r msg = TransientResourceToPeerMap {
    getTransientMap ::  HM.HashMap r (ResourceHandler r msg, TVar [NodeId])
}

data ArchivedOrTransient
    = Archived
    | Transient
    deriving (Eq, Ord, Show, Generic)

instance Serialise ArchivedOrTransient
