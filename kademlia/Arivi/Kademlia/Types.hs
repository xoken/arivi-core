-- This module defines some of the most fundamental data types that will be used
-- throughout this Kademlia implementation, there are several advantages of
-- this first being it enables the utilization of haskell's powerful type system
-- and second it makes code cleaner and structured.

{-# LANGUAGE DeriveGeneric               #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Arivi.Kademlia.Types
  (
     Message(..)
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
   , serialise
   , deserialise
  ) where

import           Arivi.Kademlia.Utils         (sockAddrToHostAddr,
                                                    sockAddrToPortNumber)
import           Codec.Serialise           (deserialise, serialise)
import           Codec.Serialise.Class     (Serialise (..))
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Crypto.Error
import           Crypto.PubKey.Ed25519     (PublicKey, SecretKey, Signature,
                                            publicKey, sign, signature)
import           Data.ByteArray            (convert)
import           Data.ByteString
import qualified Data.ByteString.Lazy      as Lazy (toStrict)
import           Data.Monoid
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)
import           Data.Word
import           GHC.Generics
import           Network.Socket

-- | Helper function to get timeStamp/ epoch
getTimeStamp :: IO TimeStamp
getTimeStamp = TimeStamp <$> getPOSIXTime

data NodeEndPoint = NodeEndPoint {
        nodeIp  :: HostAddress
    ,   udpPort :: PortNumber
    ,   tcpPort :: PortNumber
} deriving (Eq,Show,Generic)

type NodeId            = PublicKey
type Sign              = Signature
type Sequence          = Word32
newtype TimeStamp      = TimeStamp POSIXTime deriving (Show,Eq,Ord)

data MessageType = MSG01
                   |MSG02
                   |MSG03
                   |MSG04
                   deriving (Show,Generic)
-- Custom data type to send & receive message
data MessageBody = PING {
                    nodeId       :: NodeId
                ,   fromEndPoint :: NodeEndPoint
                }
               |PONG {
                    nodeId       :: NodeId
                ,   fromEndPoint :: NodeEndPoint
               }
               |FIND_NODE {
                    nodeId       :: NodeId
                ,   targetNodeId :: NodeId
                ,   fromEndPoint :: NodeEndPoint
                }
               |FN_RESP {
                    nodeId       :: NodeId
                ,   peerList     :: [(NodeId,NodeEndPoint)]
                ,   fromEndPoint :: NodeEndPoint
                }
               deriving (Generic,Show)

data Message = Message {
                     messageType :: MessageType
                ,    messageBody :: MessageBody
                ,    sequence    :: Sequence
                } deriving (Generic,Show)

data PayLoad = PayLoad {
                     message   :: Message
                ,    signature :: Sign
                } deriving (Show,Generic)


-- Helper functions to create messages
packPing :: NodeId -> SecretKey -> SockAddr -> Sequence -> PayLoad
packPing nId sk sockAddr msgSeq = PayLoad msg sgn
    where
        fromep  = NodeEndPoint (sockAddrToHostAddr sockAddr)
                    (sockAddrToPortNumber sockAddr)
                    (sockAddrToPortNumber sockAddr)
        msgBody = PING nId fromep
        msg     = Message MSG01 msgBody msgSeq
        sgn     = sign sk (nId :: PublicKey)
                    (Lazy.toStrict (serialise msg)) :: Sign

packPong :: NodeId -> SecretKey -> SockAddr -> Sequence -> PayLoad
packPong nId sk sockAddr msgSeq = PayLoad msg sgn
    where
        fromep  = NodeEndPoint (sockAddrToHostAddr sockAddr)
                    (sockAddrToPortNumber sockAddr)
                    (sockAddrToPortNumber sockAddr)
        msgBody = PONG nId fromep
        msg     = Message MSG02 msgBody msgSeq
        sgn     = sign sk (nId :: PublicKey)
                    (Lazy.toStrict (serialise msg)) ::Sign

packFindMsg :: NodeId -> SecretKey -> SockAddr -> NodeId -> Sequence -> PayLoad
packFindMsg nId sk sockAddr targetNode msgSeq  = PayLoad msg sgn
    where
        fromep  = NodeEndPoint (sockAddrToHostAddr sockAddr)
                    (sockAddrToPortNumber sockAddr)
                    (sockAddrToPortNumber sockAddr)
        msgBody = FIND_NODE nId targetNode fromep
        msg     = Message MSG03 msgBody msgSeq
        sgn     = sign sk (nId :: PublicKey)
                    (Lazy.toStrict (serialise msg)) :: Sign

packFnR :: NodeId -> SecretKey -> SockAddr -> NodeId -> Sequence -> PayLoad
packFnR nId sk sockAddr mPeerList msgSeq = PayLoad msg sgn
    where
        fromep  = NodeEndPoint (sockAddrToHostAddr sockAddr)
                    (sockAddrToPortNumber sockAddr)
                    (sockAddrToPortNumber sockAddr)
        msgBody = FIND_NODE nId mPeerList fromep
        msg     = Message MSG04 msgBody msgSeq
        sgn     = sign sk (nId :: PublicKey)
                    (Lazy.toStrict (serialise msg)) :: Sign

-- Serialise instance of different custom types so that they can be encoded
-- and decoded using serialize library which further allows these types
-- to be serialized and thus makes it possible to be sent across network

-- instance Serialise NodeId
instance Serialise NodeEndPoint
-- instance Serialise Sequence
instance Serialise PayLoad
instance Serialise MessageType
instance Serialise Message


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
    case (len,tag) of
        (2,0)  -> throwCryptoError . publicKey <$>
                    (decode :: Decoder s ByteString)
        _      -> fail "invalid PublicKey encoding"

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
    case (len,tag) of
        (2,0) ->  throwCryptoError . Crypto.PubKey.Ed25519.signature <$>
                    (decode :: Decoder s ByteString)
        _      -> fail "invalid Signature encoding"


-- Serialise Instance for SockAddr type defined in Network.Socket
instance Serialise SockAddr where
    encode = encodeSockAddr
    decode = decodeSockAddr

encodeSockAddr :: SockAddr -> Encoding
encodeSockAddr (SockAddrInet port hostip) =
    encodeListLen 3 <> encodeWord 0 <> encode port <> encode hostip
encodeSockAddr _ = error "encodeSockAddr: SockAddr is not of constructor SockAddrInet "

decodeSockAddr :: Decoder s SockAddr
decodeSockAddr = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of
        (3,0) -> SockAddrInet <$> decode <*> decode
        _     -> fail "invalid SockAddr encoding"

-- Serialise instance for PortNumber again defined in Network module
instance Serialise PortNumber where
    encode = encodePortNumber
    decode = decodePortNumber

-- | Fix warnings generated due to use of PortNum which will be deprecated by
-- | Lib

encodePortNumber :: PortNumber -> Encoding
encodePortNumber a =
    encodeListLen 2 <> encodeWord 0 <> encode a

decodePortNumber :: Decoder s PortNumber
decodePortNumber = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of
        (2,0) -> PortNum <$> decode
        _     -> fail "Invalid PortNumber encoding"


-- Serialise instance for MessageBody data type
instance Serialise MessageBody where
    encode = encodeMessageBody
    decode = decodeMessageBody

encodeMessageBody :: MessageBody -> Encoding
encodeMessageBody (PING nodeId fromEndPoint) =
    encodeListLen 3 <> encodeWord 0 <> encode nodeId <> encode fromEndPoint
encodeMessageBody (PONG nodeId toEndPoint) =
    encodeListLen 3 <> encodeWord 1 <> encode nodeId <> encode toEndPoint
encodeMessageBody (FIND_NODE nodeId targetNodeId nodeEndPoint) =
    encodeListLen 4 <> encodeWord 2 <> encode nodeId <> encode targetNodeId
                    <> encode nodeEndPoint
encodeMessageBody (FN_RESP nodeId peerList nodeEndPoint) =
    encodeListLen 4 <> encodeWord 3 <> encode nodeId <> encode peerList
                    <> encode nodeEndPoint

decodeMessageBody :: Decoder s MessageBody
decodeMessageBody = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of
        (3,0) -> PING <$> decode <*> decode
        (3,1) -> PONG <$> decode <*> decode
        (4,2) -> FIND_NODE <$> decode <*> decode <*> decode
        (4,3) -> FN_RESP <$> decode <*> decode <*> decode
        _     -> fail "Invalid MessageBody encoding"
