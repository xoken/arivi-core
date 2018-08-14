{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor,
  DeriveTraversable, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

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
module Arivi.P2P.Types (module Arivi.P2P.Types, NetworkConfig(..)) where

import           Arivi.Network.Types (NetworkConfig(..))
import           Codec.Serialise
import           GHC.Generics        (Generic)
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Proxy

type Map = HashMap



data MessageType = Kademlia
                 | Option
                 | Rpc
                 | PubSub
                 deriving (Eq, Show, Ord, Generic, Serialise, Hashable)


data Request (i :: MessageType) msg where
  RpcRequest :: (Serialise msg) => msg -> Request 'Rpc msg
  OptionRequest :: (Serialise msg) => msg -> Request 'Option msg
  KademliaRequest :: (Serialise msg) => msg -> Request 'Kademlia msg

data Response (i :: MessageType) msg where
  RpcResponse :: (Serialise msg) => msg -> Response 'Rpc msg
  OptionResponse :: (Serialise msg) => msg -> Response 'Option msg
  KademliaResponse :: (Serialise msg) => msg -> Response 'Kademlia msg


class Msg (i :: k) where
  msgType :: Proxy i -> MessageType


instance Msg i => Msg (Request i msg) where
  msgType _ = msgType (Proxy :: Proxy i)

instance (Msg i) => Msg (Response i msg) where
  msgType _ = msgType (Proxy :: Proxy i)

instance Msg 'Rpc where
  msgType _ = Rpc

instance Msg 'Option where
  msgType _ = Option

instance Msg 'Kademlia where
  msgType _ = Kademlia

data RpcPayload r msg = RpcPayload r msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data OptionPayload msg = OptionPayload msg
                       deriving (Eq, Ord, Show, Generic, Serialise)
