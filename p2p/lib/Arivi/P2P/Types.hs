{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor,
  DeriveTraversable, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    ( module Arivi.P2P.Types
    , NetworkConfig(..)
    , nodeId
    , defaultNetworkConfig
    ) where

import           Arivi.Network.Types (NetworkConfig(..), nodeId, defaultNetworkConfig)
import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding
import           GHC.Generics        (Generic)
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Proxy
import           Data.Monoid ((<>))

type Map = HashMap

data MessageType = Kademlia
                 | Option
                 | Rpc
                 | PubSub
                 deriving (Eq, Show, Ord, Generic, Serialise, Hashable)


data Request :: MessageType -> * -> * where
  RpcRequest :: (Serialise msg) => msg -> Request 'Rpc msg
  OptionRequest :: (Serialise msg) => msg -> Request 'Option msg
  KademliaRequest :: (Serialise msg) => msg -> Request 'Kademlia msg
  PubSubRequest :: (Serialise msg) => msg -> Request 'PubSub msg

data Response (i :: MessageType) msg where
  RpcResponse :: (Serialise msg) => msg -> Response 'Rpc msg
  OptionResponse :: (Serialise msg) => msg -> Response 'Option msg
  KademliaResponse :: (Serialise msg) => msg -> Response 'Kademlia msg
  PubSubResponse :: (Serialise msg) => msg -> Response 'PubSub msg

-- data PubSubMessage (i :: PubSubType) t msg where
--   PubsubPublish :: (Serialise msg, Topic t) => msg -> t ->  PubSubMessage 'Publish t msg
--   PubsubNotify :: (Serialise msg, Topic t) => msg -> t ->  PubSubMessage 'Notify t msg

-- data PubSubType = Notify | Publish deriving (Eq, Show, Ord, Generic, Serialise, Hashable)

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

instance Msg 'PubSub where
  msgType _ = PubSub


class (Serialise r) => Resource r where
    resourceId :: r -> String

instance (Resource r, Serialise msg) => Resource (RpcPayload r msg) where
  resourceId (RpcPayload r _) = resourceId r

class (Serialise t) => Topic t where
  topicId :: t -> String

instance (Topic t, Serialise msg) => Topic (PubSubPayload t msg) where
  topicId (PubSubPayload t _) = topicId t


data RpcPayload r msg = RpcPayload r msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data OptionPayload msg = OptionPayload msg
                       deriving (Eq, Ord, Show, Generic, Serialise)


-- GHC can't derive Generic instances for GADTs, so we need to write
-- serialise instances by hand. The encoding part is trvial, decoding gets
-- tricky. The following code uses a data family to propagate some type level
-- info to runtime. See https://www.well-typed.com/blog/2017/06/rtti/ . An
-- alternate approach using Singletons is also provided below

instance (HasRTTI (Request i) msg, Serialise msg) =>
         Serialise (Request i msg) where
    encode = encodeRequest
    decode = decodeRequest rtti

encodeRequest :: (Serialise msg) => Request i msg -> Encoding
encodeRequest (RpcRequest msg) = encodeListLen 2 <> encodeWord 0 <> encode msg
encodeRequest (OptionRequest msg) =
    encodeListLen 2 <> encodeWord 1 <> encode msg
encodeRequest (KademliaRequest msg) =
    encodeListLen 2 <> encodeWord 2 <> encode msg

data family RTTI (f :: k -> *) :: (k -> *)

class HasRTTI f a where
  rtti :: RTTI f a

data instance RTTI (Request i) msg where
  RttiReqRpc :: RTTI (Request 'Rpc) msg
  RttiReqKademlia :: RTTI (Request 'Kademlia) msg
  RttiReqOption :: RTTI (Request 'Option) msg

instance HasRTTI (Request 'Rpc) msg where
  rtti = RttiReqRpc

instance HasRTTI (Request 'Kademlia) msg where
  rtti = RttiReqKademlia

instance HasRTTI (Request 'Option) msg where
  rtti = RttiReqOption

decodeRequest ::
       (Serialise msg) => RTTI (Request i) msg -> Decoder s (Request i msg)
decodeRequest RttiReqRpc = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) -> RpcRequest <$> decode
        _ -> fail "Invalid RpcRequest type"
decodeRequest RttiReqOption = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 1) -> OptionRequest <$> decode
        _ -> fail "Invalid OptionRequest type"
decodeRequest RttiReqKademlia = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 2) -> KademliaRequest <$> decode
        _ -> fail "Invalid KademliaRequest type"

{-
-- Serialise instances for Request and Response GADTs using Singletons
data instance Sing (m :: MessageType) where
  SR :: Sing 'Rpc
  SO :: Sing 'Option
  SK :: Sing 'Kademlia
  SP :: Sing 'PubSub

instance SingI Rpc where
  sing = SR

instance SingI Kademlia where
  sing = SK

instance SingI Option where
  sing = SO

instance SingI Option where
  sing = SP

instance (Serialise msg, SingI i) => Serialise (Request i msg) where
    encode = encodeRequest
    decode =
        case sing :: Sing i of
            SR -> do
                len <- decodeListLen
                tag <- decodeWord
                case (len, tag) of
                    (2, 0) -> RpcRequest <$> decode
                    _ -> fail "Invalid RpcRequest type"
            SO -> do
                len <- decodeListLen
                tag <- decodeWord
                case (len, tag) of
                    (2, 1) -> OptionRequest <$> decode
                    _ -> fail "Invalid OptionRequest type"
            SK -> do
                len <- decodeListLen
                tag <- decodeWord
                case (len, tag) of
                    (2, 2) -> KademliaRequest <$> decode
                    _ -> fail "Invalid KademliaRequest type"

main = do
  let r = serialise (RpcRequest "s")
      g = deserialise r :: Request i String
  case g of
    RpcRequest m -> print m
-}
data PubSubPayload t msg = PubSubPayload t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data PubSubPublish t msg = PubSubPublish t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data PubSubNotify t msg = PubSubNotify t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)