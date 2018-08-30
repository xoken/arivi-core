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

import           Arivi.P2P.Types (RpcPayload(..), Resource)

type ServiceMessage = Lazy.ByteString
type ResourceId = String


newtype ArchivedResourceToPeerMap r = ArchivedResourceToPeerMap {
    getArchivedMap :: HM.HashMap r  (ResourceHandler, TVar [NodeId])
}

newtype ResourceToPeer = ResourceToPeer (forall r . HM.HashMap r (TVar [NodeId]))

newtype ResourceHandler = ResourceHandler (forall r m . RpcPayload r m -> RpcPayload r m)

data Options r = Options deriving (Eq, Ord, Show, Generic, Serialise)

data Supported r = Supported r deriving(Eq, Ord, Generic, Serialise, Hashable)


newtype TransientResourceToPeerMap r = TransientResourceToPeerMap {
    getTransientMap ::  HM.HashMap r (ResourceHandler, TVar [NodeId])
}

data ArchivedOrTransient
    = Archived
    | Transient
    deriving (Eq, Ord, Show, Generic)

instance Serialise ArchivedOrTransient
