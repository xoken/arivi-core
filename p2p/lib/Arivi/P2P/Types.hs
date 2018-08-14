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
import           Data.Time
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

data Response (i :: MessageType) msg where
  RpcResponse :: (Serialise msg) => msg -> Response 'Rpc msg
  OptionResponse :: (Serialise msg) => msg -> Response 'Option msg


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

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest' :: forall t i m o.(Monad m, Serialise o)
    => Request t i
    -> m (Response t o)
issueRequest' req = do
  case req of
    RpcRequest msg -> do
      let resp = deserialise (serialise msg)
      return (RpcResponse resp)
    OptionRequest r -> do
      let resp = deserialise (serialise r)
      return (OptionResponse resp)

data Resource = Block deriving (Eq, Ord, Show, Generic, Serialise)

main' = do
  RpcResponse (RpcPayload r s :: RpcPayload String Resource ) <- issueRequest' (RpcRequest (RpcPayload "abc" Block))
  print r
  print s
  

data RpcPayload r msg = RpcPayload r msg deriving (Eq, Ord, Show, Generic, Serialise)


data OptionPayload msg = OptionPayload msg deriving (Eq, Ord, Show, Generic, Serialise)

-- main :: Response Kademlia Int
-- main = deserialise (serialise (Request 1 :: Request Rpc Int))
