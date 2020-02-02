-- This module defines some of the most fundamental data types that will be used
-- throughout this Kademlia implementation, there are several advantages of
-- this first being it enables the utilization of haskell's powerful type system
-- and second it makes code cleaner and structured.
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
    , createKbucket
    , packVnR
    , packVerifyMsg
    , Peer(..)
    , Kbucket(..)
    , HasKbucket(..)
    , NodeStatus(..)
    ) where

import Arivi.P2P.Types (NetworkConfig(..))
import Codec.Serialise.Class (Serialise(..))
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad.STM (atomically)
import Crypto.Error
import Crypto.PubKey.Ed25519 (PublicKey, Signature, publicKey)
import Data.ByteArray (convert)
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy ()
import Data.Monoid ()
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word
import GHC.Generics
import Network.Socket
import qualified StmContainers.Map as H

-- | Helper function to get timeStamp/ epoch
getTimeStamp :: IO TimeStamp
getTimeStamp = TimeStamp <$> getPOSIXTime

data NodeEndPoint =
    NodeEndPoint
        { nodeIp :: HostName
        , udpPort :: PortNumber
        , tcpPort :: PortNumber
        }
    deriving (Eq, Show, Generic)

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
    | MSG05
    | MSG06
    deriving (Show, Generic)

-- | Peer information encapsulated in a single structure
data Peer =
    Peer
        { nodeID :: NodeId
        , nodeEndPoint :: NodeEndPoint
        }
    deriving (Show, Generic) --Peer (x, _) == Peer (a, _) = a == x

instance Eq Peer where
    Peer x _ == Peer a _ = a == x

-- | K-bucket to store peers
data Kbucket k v =
    Kbucket
        { getKbucket :: H.Map k v
        , nodeStatusTable :: H.Map NodeId NodeStatus
        , kademliaSoftBound :: Int
        , pingThreshold :: Int
        , kademliaConcurrencyFactor :: Int
        }

class HasKbucket m where
    getKb :: m (Kbucket Int [Peer])

-- getPeer :: NodeId -> HostName -> PortNumber -> PortNumber ->  Peer
-- getPeer nodeId ip tcpPort udpPort = (Peer nodeId (NodeEndPoint ip tcpPort udpPort))
-- | Creates a new K-bucket which is a mutable hash table, and inserts the local
-- node with position 0 i.e kb index is zero since the distance of a node
-- from it's own address is zero. This will help insert the new peers into
-- kbucket with respect to the local peer
createKbucket :: Peer -> Int -> Int -> Int -> IO (Kbucket Int [Peer])
createKbucket localPeer sbound pingThreshold' kademliaConcurrencyFactor' = do
    m <- atomically H.new
    n <- atomically H.new
    atomically $ H.insert [localPeer] 0 m
    -- atomically $ H.insert Verified (nodeID localPeer) n
    atomically $ H.insert Verified (nodeID localPeer) n
    return (Kbucket m n sbound pingThreshold' kademliaConcurrencyFactor')

-- Custom data type to send & receive message
data MessageBody
    = PING
          { nodeId :: NodeId
          , fromEndPoint :: NodeEndPoint
          }
    | PONG
          { nodeId :: NodeId
          , fromEndPoint :: NodeEndPoint
          }
    | FIND_NODE
          { nodeId :: NodeId
          , targetNodeId :: NodeId
          , fromEndPoint :: NodeEndPoint
          }
    | FN_RESP
          { nodeId :: NodeId
          , peerList :: [Peer]
          , fromEndPoint :: NodeEndPoint
          }
    | VERIFY_NODE
          { nodeId :: NodeId
          , targetNodeId :: NodeId
          , refNodeId :: NodeId
          , targetEndPoint :: NodeEndPoint
          , fromEndPoint :: NodeEndPoint
          }
    | VN_RESP
          { nodeId :: NodeId
          , peerList :: [Peer]
          , fromEndPoint :: NodeEndPoint
          }
    deriving (Generic, Show)

data Message =
    Message
        { messageType :: MessageType
        , messageBody :: MessageBody
        }
    deriving (Generic, Show)

data PayLoad =
    PayLoad
        { message :: Message
        }
    deriving (Show, Generic)

data NodeStatus
    = Verified
    | UnVerified
    deriving (Show)

-- Helper functions to create messages
packPing :: NetworkConfig -> PayLoad
packPing NetworkConfig {..} = PayLoad msg
  where
    fromep = NodeEndPoint _ip _udpPort _tcpPort
    msgBody = PING _nodeId fromep
    msg = Message MSG01 msgBody

packPong :: NetworkConfig -> PayLoad
packPong NetworkConfig {..} = PayLoad msg
  where
    fromep = NodeEndPoint _ip _udpPort _tcpPort
    msgBody = PONG _nodeId fromep
    msg = Message MSG02 msgBody

packFindMsg :: NetworkConfig -> NodeId -> PayLoad
packFindMsg NetworkConfig {..} targetNode = PayLoad msg
  where
    fromep = NodeEndPoint _ip _udpPort _tcpPort
    msgBody = FIND_NODE _nodeId targetNode fromep
    msg = Message MSG03 msgBody

packFnR :: NetworkConfig -> [Peer] -> PayLoad
packFnR NetworkConfig {..} mPeerList = PayLoad msg
  where
    fromep = NodeEndPoint _ip _udpPort _tcpPort
    msgBody = FN_RESP _nodeId mPeerList fromep
    msg = Message MSG04 msgBody

packVerifyMsg :: NetworkConfig -> NetworkConfig -> NodeId -> PayLoad
packVerifyMsg nc tnc refNode = PayLoad msg
  where
    fromep = NodeEndPoint (_ip nc) (_udpPort nc) (_tcpPort nc)
    targetep = NodeEndPoint (_ip tnc) (_udpPort tnc) (_tcpPort tnc)
    msgBody = VERIFY_NODE (_nodeId nc) (_nodeId tnc) refNode targetep fromep
    msg = Message MSG05 msgBody

packVnR :: NetworkConfig -> [Peer] -> PayLoad
packVnR NetworkConfig {..} mPeerList = PayLoad msg
  where
    fromep = NodeEndPoint _ip _udpPort _tcpPort
    msgBody = VN_RESP _nodeId mPeerList fromep
    msg = Message MSG06 msgBody

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
        (2, 0) -> throwCryptoError . publicKey <$> (decode :: Decoder s ByteString)
        _ -> fail "invalid PublicKey encoding"

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
        _ -> fail "Invalid PortNumber encoding"

-- Serialise instance for MessageBody data type
instance Serialise MessageBody where
    encode = encodeMessageBody
    decode = decodeMessageBody

encodeMessageBody :: MessageBody -> Encoding
encodeMessageBody (PING pnodeId pfromEndPoint) =
    encodeListLen 3 <> encodeWord 0 <> encode pnodeId <> encode pfromEndPoint
encodeMessageBody (PONG pnodeId ptoEndPoint) = encodeListLen 3 <> encodeWord 1 <> encode pnodeId <> encode ptoEndPoint
encodeMessageBody (FIND_NODE pnodeId ptargetNodeId pnodeEndPoint) =
    encodeListLen 4 <> encodeWord 2 <> encode pnodeId <> encode ptargetNodeId <> encode pnodeEndPoint
encodeMessageBody (FN_RESP pnodeId ppeerList pnodeEndPoint) =
    encodeListLen 4 <> encodeWord 3 <> encode pnodeId <> encode ppeerList <> encode pnodeEndPoint
encodeMessageBody (VERIFY_NODE pnodeId ptargetNodeId prefNodeId tnodeEndPoint pnodeEndPoint) =
    encodeListLen 6 <> encodeWord 4 <> encode pnodeId <> encode ptargetNodeId <> encode prefNodeId <>
    encode tnodeEndPoint <>
    encode pnodeEndPoint
encodeMessageBody (VN_RESP pnodeId ppeerList pnodeEndPoint) =
    encodeListLen 4 <> encodeWord 5 <> encode pnodeId <> encode ppeerList <> encode pnodeEndPoint

decodeMessageBody :: Decoder s MessageBody
decodeMessageBody = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (3, 0) -> PING <$> decode <*> decode
        (3, 1) -> PONG <$> decode <*> decode
        (4, 2) -> FIND_NODE <$> decode <*> decode <*> decode
        (4, 3) -> FN_RESP <$> decode <*> decode <*> decode
        (6, 4) -> VERIFY_NODE <$> decode <*> decode <*> decode <*> decode <*> decode
        (4, 5) -> VN_RESP <$> decode <*> decode <*> decode
        _ -> fail "Invalid MessageBody encoding"
