-- This module defines some of the most fundamental data types that will be used
-- throughout this Kademlia implementation, there are several advantages of
-- this first being it enables the utilization of haskell's powerful type system
-- and second it makes code cleaner and structured.
{-# LANGUAGE DeriveGeneric    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Arivi.P2P.Kademlia.Types
    ( Message(..)
    , MessageBody(..)
    , MessageType(..)
    , NodeEndPoint(..)
    , NodeId
    , PayLoad(..)
    , Sign
    , Sequence
    , TimeStamp
    , getTimeStamp
    , packFindMsg
    , packFnR
    , packPing
    , packPong
    -- , serialise
    -- , deserialise
    , Peer(..)
    , Kbucket(..)
    , HasKbucket(..)
    , PeerStatus(..)
    ) where

import           Codec.Serialise.Class       (Serialise (..))
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Error
import           Crypto.PubKey.Ed25519       (PublicKey, Signature, publicKey,
                                              signature)
import           Data.ByteArray              (convert)
import           Data.ByteString
import qualified Data.ByteString.Char8       as C
import           Data.ByteString.Lazy        ()
import           Data.Monoid
import           Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime)
import           Data.Word
import           GHC.Generics
import           Network.Socket
import qualified STMContainers.Map           as H

-- | Helper function to get timeStamp/ epoch
getTimeStamp :: IO TimeStamp
getTimeStamp = TimeStamp <$> getPOSIXTime

data NodeEndPoint = NodeEndPoint
    { nodeIp  :: HostName
    , udpPort :: PortNumber
    , tcpPort :: PortNumber
    } deriving (Eq, Show, Generic)

type NodeId = C.ByteString

type Sign = Signature

type Sequence = Word32

newtype TimeStamp =
    TimeStamp POSIXTime
    deriving (Show, Eq, Ord)

data MessageType
    = MSG01
    | MSG02
    | MSG03
    | MSG04
    deriving (Show, Generic)

-- | Peer information encapsulated in a single structure
newtype Peer = Peer
    { getPeer :: (NodeId, NodeEndPoint)
    } deriving (Show, Generic)

data PeerStatus
    = Active
    | Stale
    deriving (Show)

instance Eq Peer where
    Peer (x, _) == Peer (a, _) = a == x

-- | K-bucket to store peers
newtype Kbucket k v = Kbucket
    { getKbucket :: H.Map k v
    }

class (MonadIO m, MonadBaseControl IO m) =>
      HasKbucket m
    where
    getKb :: m (Kbucket Int [(Peer, PeerStatus)])

-- Custom data type to send & receive message
data MessageBody
    = PING { nodeId       :: NodeId
           , fromEndPoint :: NodeEndPoint }
    | PONG { nodeId       :: NodeId
           , fromEndPoint :: NodeEndPoint }
    | FIND_NODE { nodeId       :: NodeId
                , targetNodeId :: NodeId
                , fromEndPoint :: NodeEndPoint }
    | FN_RESP { nodeId       :: NodeId
              , peerList     :: [Peer]
              , fromEndPoint :: NodeEndPoint }
    deriving (Generic, Show)

data Message = Message
    { messageType :: MessageType
    , messageBody :: MessageBody
    } deriving (Generic, Show)

data PayLoad = PayLoad
    { message :: Message
    } deriving (Show, Generic)

-- Helper functions to create messages
packPing :: NodeId -> HostName -> PortNumber -> PortNumber -> PayLoad
packPing nId hostName udpPortb tcpPortb = PayLoad msg
  where
    fromep = NodeEndPoint hostName udpPortb tcpPortb
    msgBody = PING nId fromep
    msg = Message MSG01 msgBody

packPong :: NodeId -> HostName -> PortNumber -> PortNumber -> PayLoad
packPong nId hostName udpPort' tcpPort' = PayLoad msg
  where
    fromep = NodeEndPoint hostName udpPort' tcpPort'
    msgBody = PONG nId fromep
    msg = Message MSG02 msgBody

packFindMsg ::
       NodeId -> NodeId -> HostName -> PortNumber -> PortNumber -> PayLoad
packFindMsg nId targetNode hostName' udpPort'' tcpPort'' = PayLoad msg
  where
    fromep = NodeEndPoint hostName' udpPort'' tcpPort''
    msgBody = FIND_NODE nId targetNode fromep
    msg = Message MSG03 msgBody

packFnR :: NodeId -> [Peer] -> HostName -> PortNumber -> PortNumber -> PayLoad
packFnR nId mPeerList hostNamea udpPorta tcpPorta = PayLoad msg
  where
    fromep = NodeEndPoint hostNamea udpPorta tcpPorta
    msgBody = FN_RESP nId mPeerList fromep
    msg = Message MSG04 msgBody

-- Serialise instance of different custom types so that they can be encoded
-- and decoded using serialize library which further allows these types
-- to be serialized and thus makes it possible to be sent across network
-- instance Serialise NodeId
instance Serialise NodeEndPoint

-- instance Serialise Sequence
instance Serialise PayLoad

instance Serialise MessageType

instance Serialise Message

instance Serialise Peer

-- Serialise instance for PublicKey
instance Serialise PublicKey where
    encode = encodePublicKey
    decode = decodePublicKey

encodePublicKey :: PublicKey -> Encoding
encodePublicKey bytes = do
    let temp = convert bytes :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodePublicKey :: Decoder s PublicKey
decodePublicKey = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) ->
            throwCryptoError . publicKey <$> (decode :: Decoder s ByteString)
        _ -> fail "invalid PublicKey encoding"

-- Serilaise instance for Signature
instance Serialise Signature where
    encode = encodeSignature
    decode = decodeSignature

encodeSignature :: Signature -> Encoding
encodeSignature bytes = do
    let temp = convert bytes :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodeSignature :: Decoder s Signature
decodeSignature = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) ->
            throwCryptoError . Crypto.PubKey.Ed25519.signature <$>
            (decode :: Decoder s ByteString)
        _ -> fail "invalid Signature encoding"

-- Serialise Instance for SockAddr type defined in Network.Socket
instance Serialise SockAddr where
    encode = encodeSockAddr
    decode = decodeSockAddr

encodeSockAddr :: SockAddr -> Encoding
encodeSockAddr (SockAddrInet port hostip) =
    encodeListLen 3 <> encodeWord 0 <> encode port <> encode hostip
encodeSockAddr _ =
    error "encodeSockAddr: SockAddr is not of constructor SockAddrInet "

decodeSockAddr :: Decoder s SockAddr
decodeSockAddr = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (3, 0) -> SockAddrInet <$> decode <*> decode
        _      -> fail "invalid SockAddr encoding"

-- Serialise instance for PortNumber again defined in Network module
instance Serialise PortNumber where
    encode = encodePortNumber
    decode = decodePortNumber

-- | Fix warnings generated due to use of PortNum which will be deprecated by
-- | Lib
encodePortNumber :: Integral a => a -> Encoding
encodePortNumber a = encodeListLen 2 <> encodeWord 0 <> encode (toInteger a)

decodePortNumber :: Decoder s PortNumber
decodePortNumber = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) -> fromInteger <$> decode
        _      -> fail "Invalid PortNumber encoding"

-- Serialise instance for MessageBody data type
instance Serialise MessageBody where
    encode = encodeMessageBody
    decode = decodeMessageBody

encodeMessageBody :: MessageBody -> Encoding
encodeMessageBody (PING pnodeId pfromEndPoint) =
    encodeListLen 3 <> encodeWord 0 <> encode pnodeId <> encode pfromEndPoint
encodeMessageBody (PONG pnodeId ptoEndPoint) =
    encodeListLen 3 <> encodeWord 1 <> encode pnodeId <> encode ptoEndPoint
encodeMessageBody (FIND_NODE pnodeId ptargetNodeId pnodeEndPoint) =
    encodeListLen 4 <> encodeWord 2 <> encode pnodeId <> encode ptargetNodeId <>
    encode pnodeEndPoint
encodeMessageBody (FN_RESP pnodeId ppeerList pnodeEndPoint) =
    encodeListLen 4 <> encodeWord 3 <> encode pnodeId <> encode ppeerList <>
    encode pnodeEndPoint

decodeMessageBody :: Decoder s MessageBody
decodeMessageBody = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (3, 0) -> PING <$> decode <*> decode
        (3, 1) -> PONG <$> decode <*> decode
        (4, 2) -> FIND_NODE <$> decode <*> decode <*> decode
        (4, 3) -> FN_RESP <$> decode <*> decode <*> decode
        _      -> fail "Invalid MessageBody encoding"
