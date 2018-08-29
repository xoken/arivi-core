{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor,
  DeriveTraversable, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds #-}

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

-- RpcA is just a random name for an Rpc message subtype
data MessageSubType = None | RpcA

data Request :: MessageType -> MessageSubType -> * -> * where
  RpcRequest :: (Serialise msg) => msg -> Request 'Rpc 'RpcA msg
  OptionRequest :: (Serialise msg) => msg -> Request 'Option 'None msg
  KademliaRequest :: (Serialise msg) => msg -> Request 'Kademlia 'None msg
  PubSubRequest :: (Serialise msg) => msg -> Request 'PubSub 'None msg

data Response (i :: MessageType) (j :: MessageSubType) msg where
  RpcResponse :: (Serialise msg) => msg -> Response 'Rpc 'RpcA msg
  OptionResponse :: (Serialise msg) => msg -> Response 'Option 'None msg
  KademliaResponse :: (Serialise msg) => msg -> Response 'Kademlia 'None msg
  PubSubResponse :: (Serialise msg) => msg -> Response 'PubSub 'None msg

-- data PubSubMessage (i :: PubSubType) t msg where
--   PubsubPublish :: (Serialise msg, Topic t) => msg -> t ->  PubSubMessage 'Publish t msg
--   PubsubNotify :: (Serialise msg, Topic t) => msg -> t ->  PubSubMessage 'Notify t msg

-- data PubSubType = Notify | Publish deriving (Eq, Show, Ord, Generic, Serialise, Hashable)

class Msg (i :: k) (j :: k1) where
  msgType :: Proxy i -> Proxy j -> (MessageType, MessageSubType)
  -- msgSubType :: Proxy j -> MessageSubType

instance Msg i j => Msg (Request i j msg) k where
  msgType _ _ = msgType (Proxy :: Proxy i) (Proxy :: Proxy j)
  -- msgSubType _ = msgSubType (Proxy :: Proxy j)

instance Msg i j => Msg (Response i j msg) k where
  msgType _ _ = msgType (Proxy :: Proxy i) (Proxy :: Proxy j)
  -- msgSubType _ = msgSubType (Proxy :: Proxy j)

instance Msg 'Rpc 'RpcA where
  msgType _ _ = (Rpc, RpcA)
  -- msgSubType _ = RpcA

instance Msg 'Option 'None where
  msgType _ _ = (Option, None)

instance Msg 'Kademlia 'None where
  msgType _ _ = (Kademlia, None)

instance Msg 'PubSub 'None where
  msgType _ _ = (PubSub, None)


type Resource r = (Eq r, Hashable r, Serialise r)

-- class (Eq r, Hashable r, Serialise r) => Resource r where
--     resourceId :: r -> String

-- instance (Resource r, Serialise msg) => Resource (RpcPayload r msg) where
--   resourceId (RpcPayload r _) = resourceId r

class (Serialise t) => Topic t where
  topicId :: t -> String


data RpcPayload r msg = RpcPayload r msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data OptionPayload msg = OptionPayload msg
                       deriving (Eq, Ord, Show, Generic, Serialise)


data PubSubPayload t msg = PubSubPayload t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data PubSubPublish t msg = PubSubPublish t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

data PubSubNotify t msg = PubSubNotify t msg
                      deriving (Eq, Ord, Show, Generic, Serialise)

instance (Topic t, Serialise msg) => Topic (PubSubPayload t msg) where
  topicId (PubSubPayload t _) = topicId t

data family RTTI (f :: k -> *) :: (k -> *)

class HasRTTI f a where
  rtti :: RTTI f a

data instance RTTI (Request i j) msg where
  RttiReqRpcA :: RTTI (Request 'Rpc 'RpcA) msg
  RttiReqKademlia :: RTTI (Request 'Kademlia 'None) msg
  RttiReqOption :: RTTI (Request 'Option 'None) msg
  RttiReqPubSub :: RTTI (Request 'PubSub 'None) msg

instance HasRTTI (Request 'Rpc 'RpcA) msg where
  rtti = RttiReqRpcA

instance HasRTTI (Request 'Kademlia 'None) msg where
  rtti = RttiReqKademlia

instance HasRTTI (Request 'Option 'None) msg where
  rtti = RttiReqOption


instance HasRTTI (Request 'PubSub 'None) msg where
  rtti = RttiReqPubSub

encodeRequest :: (Serialise msg) => Request i j msg -> Encoding
encodeRequest (RpcRequest msg) = encodeListLen 2 <> encodeWord 0 <> encode msg
encodeRequest (OptionRequest msg) =
    encodeListLen 2 <> encodeWord 1 <> encode msg
encodeRequest (KademliaRequest msg) =
    encodeListLen 2 <> encodeWord 2 <> encode msg
encodeRequest (PubSubRequest msg) =
    encodeListLen 2 <> encodeWord 3 <> encode msg

decodeRequest ::
       (Serialise msg) => RTTI (Request i j) msg -> Decoder s (Request i j msg)
decodeRequest RttiReqRpcA = do
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

decodeRequest RttiReqPubSub = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 3) -> PubSubRequest <$> decode
        _ -> fail "Invalid PubSubRequest type"

-- GHC can't derive Generic instances for GADTs, so we need to write
-- serialise instances by hand. The encoding part is trvial, decoding gets
-- tricky. The following code uses a data family to propagate some type level
-- info to runtime. See https://www.well-typed.com/blog/2017/06/rtti/ . An
-- alternate approach using Singletons is also provided below

instance (HasRTTI (Request i j) msg, Serialise msg) =>
         Serialise (Request i j msg) where
    encode = encodeRequest
    decode = decodeRequest rtti

data instance  RTTI (Response i j) msg where
        RttiResRpcA :: RTTI (Response 'Rpc 'RpcA) msg
        RttiResOption :: RTTI (Response 'Option 'None) msg
        RttiResKademlia :: RTTI (Response 'Kademlia 'None) msg
        RttiResPubSub :: RTTI (Response 'PubSub 'None) msg

instance HasRTTI (Response 'Rpc 'RpcA) msg where
    rtti = RttiResRpcA

instance HasRTTI (Response 'Option 'None) msg where
    rtti = RttiResOption

instance HasRTTI (Response 'Kademlia 'None) msg where
    rtti = RttiResKademlia

instance HasRTTI (Response 'PubSub 'None) msg where
    rtti = RttiResPubSub

encodeResponse :: (Serialise msg) => Response i j msg -> Encoding
encodeResponse (RpcResponse msg) = encodeListLen 2 <> encodeWord 0 <> encode msg
encodeResponse (OptionResponse msg) =
    encodeListLen 2 <> encodeWord 1 <> encode msg
encodeResponse (KademliaResponse msg) =
    encodeListLen 2 <> encodeWord 2 <> encode msg
encodeResponse (PubSubResponse msg) =
    encodeListLen 2 <> encodeWord 3 <> encode msg

decodeResponse ::
       (Serialise msg) => RTTI (Response i j) msg -> Decoder s (Response i j msg)
decodeResponse RttiResRpcA = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) -> RpcResponse <$> decode
        _ -> fail "Failed to deserialise into a valid RpcResponse type"
decodeResponse RttiResOption = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 1) -> OptionResponse <$> decode
        _ -> fail "Failed to deserialise into a valid OptionResponse type"
decodeResponse RttiResKademlia = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 2) -> KademliaResponse <$> decode
        _ -> fail "Failed to deserialise into a valid KademliaResponse type"

decodeResponse RttiResPubSub = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 3) -> PubSubResponse <$> decode
        _ -> fail "Failed to deserialise into a valid PubSubResponse type"

instance (HasRTTI (Response i j) msg, Serialise msg) =>
         Serialise (Response i j msg) where
    encode = encodeResponse
    decode = decodeResponse rtti




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
