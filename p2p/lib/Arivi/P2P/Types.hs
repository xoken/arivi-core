{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

data PubSub
    = Notify
    | Publish
    | Subscribe
    deriving (Eq, Show, Ord, Generic, Serialise)

data MessageType = Kademlia
                 | Option
                 | Rpc
                 | PubSub PubSub
                 deriving (Eq, Show, Ord, Generic, Serialise)

data Request :: MessageType -> * -> * where
    RpcRequest :: msg -> Request 'Rpc msg
    OptionRequest :: msg -> Request 'Option msg
    KademliaRequest :: msg -> Request 'Kademlia msg
    PubSubRequest :: msg -> Request ('PubSub i) msg

data Response :: MessageType -> * -> * where
    RpcResponse :: msg -> Response 'Rpc msg
    OptionResponse :: msg -> Response 'Option msg
    KademliaResponse :: msg -> Response 'Kademlia msg
    PubSubResponse :: msg -> Response ('PubSub i) msg

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

instance Msg ('PubSub 'Notify) where
  msgType _ = PubSub Notify

instance Msg ('PubSub 'Publish) where
  msgType _ = PubSub Publish

instance Msg ('PubSub 'Subscribe) where
  msgType _ = PubSub Subscribe

type Resource r = (Eq r, Hashable r, Serialise r)

data Error = ResourceNotFound deriving (Eq, Ord, Show, Generic, Serialise)

data RpcPayload r msg = RpcPayload r msg
                      | RpcError Error 
                      deriving (Eq, Ord, Show, Generic, Serialise)

data OptionPayload msg = OptionPayload msg
                       deriving (Eq, Ord, Show, Generic, Serialise)


newtype PubSubPayload t msg = PubSubPayload (t, msg)
                      deriving (Eq, Ord, Show, Generic, Serialise)

data family RTTI (f :: k -> *) :: (k -> *)

class HasRTTI f a where
  rtti :: RTTI f a

data instance RTTI (Request i) msg where
  RttiReqRpc :: RTTI (Request 'Rpc) msg
  RttiReqKademlia :: RTTI (Request 'Kademlia) msg
  RttiReqOption :: RTTI (Request 'Option) msg
  RttiReqPubSub :: RTTI (Request ('PubSub i)) msg

instance HasRTTI (Request 'Rpc) msg where
  rtti = RttiReqRpc

instance HasRTTI (Request 'Kademlia) msg where
  rtti = RttiReqKademlia

instance HasRTTI (Request 'Option) msg where
  rtti = RttiReqOption


instance HasRTTI (Request ('PubSub i)) msg where
  rtti = RttiReqPubSub

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
encodeRequest (PubSubRequest msg) =
    encodeListLen 2 <> encodeWord 3 <> encode msg

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

decodeRequest RttiReqPubSub = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 3) -> PubSubRequest <$> decode
        _ -> fail "Invalid PubSubRequest type"

data instance  RTTI (Response i) msg where
        RttiResRpc :: RTTI (Response 'Rpc) msg
        RttiResOption :: RTTI (Response 'Option) msg
        RttiResKademlia :: RTTI (Response 'Kademlia) msg
        RttiResPubSub :: RTTI (Response ('PubSub i)) msg

instance HasRTTI (Response 'Rpc) msg where
    rtti = RttiResRpc

instance HasRTTI (Response 'Option) msg where
    rtti = RttiResOption

instance HasRTTI (Response 'Kademlia) msg where
    rtti = RttiResKademlia

instance HasRTTI (Response ('PubSub i)) msg where
    rtti = RttiResPubSub

instance (HasRTTI (Response i) msg, Serialise msg) =>
         Serialise (Response i msg) where
    encode = encodeResponse
    decode = decodeResponse rtti

encodeResponse :: (Serialise msg) => Response i msg -> Encoding
encodeResponse (RpcResponse msg) = encodeListLen 2 <> encodeWord 0 <> encode msg
encodeResponse (OptionResponse msg) =
    encodeListLen 2 <> encodeWord 1 <> encode msg
encodeResponse (KademliaResponse msg) =
    encodeListLen 2 <> encodeWord 2 <> encode msg
encodeResponse (PubSubResponse msg) =
    encodeListLen 2 <> encodeWord 3 <> encode msg

decodeResponse ::
       (Serialise msg) => RTTI (Response i) msg -> Decoder s (Response i msg)
decodeResponse RttiResRpc = do
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


deriving instance (Eq i) => Eq (Request t i)

compareRequest :: (Ord i) => Request t i -> Request t i -> Ordering
compareRequest (RpcRequest a) (RpcRequest b) = compare a b
compareRequest (KademliaRequest a) (KademliaRequest b) = compare a b
compareRequest (OptionRequest a) (OptionRequest b) = compare a b
compareRequest (PubSubRequest a) (PubSubRequest b) = compare a b

instance (Ord i) => Ord (Request t i) where
  compare = compareRequest


deriving instance (Eq i) => Eq (Response t i)

compareResponse :: (Ord i) => Response t i -> Response t i -> Ordering
compareResponse (RpcResponse a) (RpcResponse b) = compare a b
compareResponse (KademliaResponse a) (KademliaResponse b) = compare a b
compareResponse (OptionResponse a) (OptionResponse b) = compare a b
compareResponse (PubSubResponse a) (PubSubResponse b) = compare a b

instance (Ord i) => Ord (Response t i) where
  compare = compareResponse

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
