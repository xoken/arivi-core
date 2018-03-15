-- This module defines some of the most fundamental data types that will be used 
-- throughout this kademlia implementation, there are sevelral advantages of 
-- this first being it enables the utilization of haskell's poweful type system 
-- and scond it makes code cleaner and structured.  

{-# LANGUAGE DeriveGeneric #-}
module Kademlia.Types 
  ( getTimeStamp,
    Message (Message),
    messageType,
    messageBody,
    Kademlia.Types.sequence,
    nodeId,
    fromEndPoint,
    targetNodeId,
    nodeIp,
    udpPort,
    tcpPort,
    message,
    peerList, 
    Kademlia.Types.signature,
    Kademlia.Types.Sequence,
    NodeId,
    Sign, 
    NodeEndPoint(NodeEndPoint),
    TimeStamp(TimeStamp),
    MessageType(MSG01,MSG02,MSG03,MSG04),
    MessageBody(PING,PONG,FIND_NODE,FN_RESP),
    PayLoad (PayLoad),
    packFindMsg,
    packFnR,
    packPing,
    packPong
  ) where 
   
import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding
import           Codec.Serialise.Class
import           Data.Monoid 
import qualified Data.ByteString.Lazy as BSL
import           GHC.Generics
import           Network.Socket            hiding (recv)
import qualified Network.Socket.ByteString as N (recv, recvFrom, sendAll,
                                                 sendAllTo, sendTo)
import qualified Network.Socket.Internal   as M 
import qualified Data.List.Split           as S  
import           Data.Word 
import           Data.Time.Clock.POSIX 
import           Data.Time.Clock           
import           Kademlia.Utils 
import           Data.ByteString
import           Crypto.PubKey.Ed25519
import           Data.ByteArray 
import           Crypto.Error 

import qualified Data.ByteString.Lazy as LBS  

-- Helper function to get timeStamp/ epoch 
getTimeStamp :: IO TimeStamp
getTimeStamp = do 
    tStamp <- getPOSIXTime 
    return $ TimeStamp (posixSecondsToUTCTime tStamp) 

data NodeEndPoint = NodeEndPoint {
        nodeIp  :: HostAddress
    ,   udpPort :: PortNumber
    ,   tcpPort :: PortNumber 
} deriving (Eq,Show,Generic)

type NodeId            = PublicKey
type Sign              = Signature
type Sequence          = Int
newtype TimeStamp      = TimeStamp UTCTime deriving (Show,Generic)

data MessageType = MSG01
                   |MSG02
                   |MSG03 
                   |MSG04 
                   deriving (Show,Generic)
-- Custom data type to send & recieve message  
data MessageBody = PING {
                    nodeId         :: NodeId
                ,   fromEndPoint   :: NodeEndPoint
                -- ,   toEndPoint     :: NodeEndPoint  
                }
               |PONG {
                    nodeId         :: NodeId
                ,   fromEndPoint   :: NodeEndPoint
             -- ,   toEndPoint     :: NodeEndPoint 
               }
               |FIND_NODE { 
                    nodeId         :: NodeId
                ,   targetNodeId   :: NodeId
                ,   fromEndPoint   :: NodeEndPoint
                }
               |FN_RESP {
                    nodeId         :: NodeId
                ,   peerList       :: [(NodeId,NodeEndPoint)]
                ,   fromEndPoint   :: NodeEndPoint 
                }            
               deriving (Generic,Show)

data Message = Message {
                     messageType   :: MessageType  
                ,    messageBody   :: MessageBody  
                ,    sequence      :: Sequence 
                } deriving (Generic,Show)

data PayLoad = PayLoad {
                     message       :: Message 
                ,    signature     :: Sign   
                } deriving (Show,Generic) 

-- Heper functions to create messages 
packPing nodeId sk sockAddr msgSeq = PayLoad msg sgn 
    where 
        fromep   = NodeEndPoint (sockAddrToHostAddr sockAddr) (sockAddrToPortNumber sockAddr) (sockAddrToPortNumber sockAddr) 
        msgBody = PING nodeId fromep 
        msg     = Message (MSG01) msgBody msgSeq 
        sgn     = (sign (sk) (nodeId :: PublicKey) (LBS.toStrict (serialise(msg)) )) :: Sign

packPong nodeId sk sockAddr msgSeq = PayLoad msg sgn 
    where 
        fromep  = NodeEndPoint (sockAddrToHostAddr sockAddr) (sockAddrToPortNumber sockAddr) (sockAddrToPortNumber sockAddr) 
        msgBody = PONG nodeId fromep
        msg     = Message (MSG02) msgBody msgSeq 
        sgn     = (sign (sk) (nodeId :: PublicKey) (LBS.toStrict (serialise(msg)) )) :: Sign

packFindMsg nodeId sk sockAddr msgSeq targetNode = PayLoad msg sgn 
    where 
        fromep   = NodeEndPoint (sockAddrToHostAddr sockAddr) (sockAddrToPortNumber sockAddr) (sockAddrToPortNumber sockAddr) 
        msgBody = FIND_NODE nodeId targetNode fromep 
        msg     = Message (MSG03) msgBody msgSeq 
        sgn     = (sign (sk) (nodeId :: PublicKey) (LBS.toStrict (serialise(msg)) )) :: Sign

packFnR nodeId sk sockAddr msgSeq peerList = PayLoad msg sgn 
    where 
        fromep   = NodeEndPoint (sockAddrToHostAddr sockAddr) (sockAddrToPortNumber sockAddr) (sockAddrToPortNumber sockAddr) 
        msgBody = FIND_NODE nodeId peerList fromep 
        msg     = Message (MSG04) msgBody msgSeq 
        sgn     = (sign (sk) (nodeId :: PublicKey) (LBS.toStrict (serialise(msg)) )) :: Sign


-- Serialise instance of different custom types so that they can be encoded 
-- and decoded using serialise library which further allows these types 
-- to be serialised and thuse makes it possible to be sent across network 

-- instance Serialise NodeId 
instance Serialise NodeEndPoint 
-- instance Serialise Sequence
instance Serialise TimeStamp 
instance Serialise PayLoad 
instance Serialise MessageType 
instance Serialise Message

-- instance Eq MessageType where 
--     MSG01 == MSG01 = True
--     MSG02 == MSG02 = True
--     MSG03 == MSG03 = True
--     MSG04 == MSG04 = True
--     _ == _         = False 

-- Serialise intance for PublicKey 
instance Serialise PublicKey where 
    encode = encodePublicKey 
    decode = decodePublicKey 

encodePublicKey :: PublicKey -> Encoding
encodePublicKey (bytes) = do
    let temp = convert bytes :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp   

decodePublicKey :: Decoder s PublicKey 
decodePublicKey = do 
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of 
        (2,0)  -> throwCryptoError <$> publicKey <$> (decode :: Decoder s ByteString)
        _      -> fail "invalid PublicKey encoding"

-- Serilaise instance for Signature 
instance Serialise Signature where
    encode = encodeSignature
    decode = decodeSignature 

encodeSignature :: Signature -> Encoding
encodeSignature (bytes) = do
    let temp = convert bytes :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp  

decodeSignature :: Decoder s Signature
decodeSignature = do 
    len <- decodeListLen
    tag <- decodeWord 
    case (len,tag) of 
        (2,0) ->  throwCryptoError <$> Crypto.PubKey.Ed25519.signature <$> (decode :: Decoder s ByteString)
        _      -> fail "invalid Signature encoding"
        

-- Serialise Instance for SockAddr type defined in Network.Socket 
instance Serialise SockAddr where 
    encode = encodeSockAddr
    decode = decodeSockAddr  

encodeSockAddr :: SockAddr -> Encoding
encodeSockAddr (SockAddrInet port hostip) = 
    encodeListLen 3 <> encodeWord 0 <> encode port <> encode hostip 

decodeSockAddr :: Decoder s SockAddr 
decodeSockAddr = do 
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of 
        (3,0) -> SockAddrInet <$> decode <*> decode 
        _      -> fail "invalid SockAddr encoding"

-- Serialise instance for PortNumber again defined in Network module 
instance Serialise PortNumber where 
    encode = encodePortNumber 
    decode = decodePortNumber 

encodePortNumber :: PortNumber -> Encoding
encodePortNumber (PortNum a) = 
    encodeListLen 2 <> encodeWord 0 <> encode a 

decodePortNumber :: Decoder s PortNumber
decodePortNumber = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of 
        (2,0)  -> (PortNum <$> decode)
        _      -> fail "Invalid PortNumber encoding"  

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
    encodeListLen 4 <> encodeWord 2 <> encode nodeId <> encode targetNodeId <> encode nodeEndPoint 
encodeMessageBody (FN_RESP nodeId peerList nodeEndPoint) = 
    encodeListLen 4 <> encodeWord 3 <> encode nodeId <> encode peerList <> encode nodeEndPoint 

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
