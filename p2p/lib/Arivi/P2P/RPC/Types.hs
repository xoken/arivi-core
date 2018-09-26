{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}

module Arivi.P2P.RPC.Types
    ( module Arivi.P2P.RPC.Types
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Data.HashMap.Strict                   as HM
import           Data.Hashable
import           Data.Set                              (Set)
import           GHC.Generics                          (Generic)


newtype Resourcers r = Resourcers (HM.HashMap r (TVar (Set NodeId)))

newtype ResourceHandler msg = ResourceHandler (forall m. (MonadIO m) => msg -> m msg)

data Options r = Options deriving (Eq, Ord, Show, Generic, Serialise)

data Supported r = Supported r deriving(Eq, Ord, Generic, Serialise, Hashable)
