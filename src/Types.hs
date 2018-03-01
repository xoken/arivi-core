-- This module defines some of the most fundamental data types that will be used 
-- throughout this kademlia implementation, there are sevelral advantages of 
-- this first being it enables the utilization of haskell's poweful type system 
-- and scond it makes code cleaner and structured.  

{-# LANGUAGE DeriveGeneric #-}
module Types 
  ( getTimeStamp,
    Message,
    NodeId(NodeId),
    NodeEndPoint(NodeEndPoint),
    Signature(Signature),
    Sequence(Sequence),
    TimeStamp(TimeStamp)
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
import           Utils 

data NodeEndPoint = NodeEndPoint {
        nodeIp  :: HostAddress
    ,   udpPort :: PortNumber
    ,   tcpPort :: PortNumber 
} deriving (Show,Generic)

newtype NodeId         = NodeId String deriving (Show,Generic)
newtype Signature      = Signature String deriving (Show,Generic)
newtype Sequence       = Sequence Int deriving (Show,Generic)
newtype TimeStamp      = TimeStamp (UTCTime) deriving (Show,Generic)

-- Helper function to get timeStamp/ epoch 
getTimeStamp :: IO TimeStamp
getTimeStamp = do 
    tStamp <- getPOSIXTime 
    return $ TimeStamp (posixSecondsToUTCTime tStamp)

data MessageType = MSG01
                   |MSG02
                   |MSG03 
                   |MSG04 
                   deriving (Show,Generic)
-- Custom data type to send & recieve message  
data Message = PING {
                    nodeId         :: NodeId
                ,   fromEndPoint   :: NodeEndPoint
                ,   toEndPoint     :: NodeEndPoint  
                }
               |PONG {
                    nodeId         :: NodeId
                ,   toEndPoint     :: NodeEndPoint 
               }
               |FIND_NODE { 
                    nodeId         :: NodeId
                ,   targetNodeId   :: NodeId
                ,   nodeEndPoint   :: NodeEndPoint
                }
               |FN_RESP {
                    nodeId         :: NodeId
                ,   peerList       :: [(NodeEndPoint,NodeId)] 
                ,   nodeEndPoint   :: NodeEndPoint 
                }            
               deriving (Generic,Show)

data Payload = Payload {
         messageType :: MessageType  
    ,    message     :: Message 
    ,    sequence    :: Sequence
    ,    timeStamp   :: TimeStamp
    ,    signature   :: Signature    
} deriving (Show,Generic) 

-- Serialise instance of different custom types so that they can be encoded 
-- and decoded using serialise library which further allows these types 
-- to be serialised and thuse makes it possible to be sent across network 

instance Serialise NodeId 
instance Serialise NodeEndPoint 
instance Serialise Signature
instance Serialise Sequence
instance Serialise TimeStamp 
instance Serialise Payload 
instance Serialise MessageType 

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
        (2,0)  -> PortNum <$> decode
        _      -> fail "Invalid PortNumber encoding"  

-- Serialise instance for Message data type
instance Serialise Message where 
    encode = encodeMessage 
    decode = decodeMessage 

encodeMessage :: Message -> Encoding 
encodeMessage (PING nodeId toEndPoint fromEndPoint) = 
    encodeListLen 4 <> encodeWord 0 <> encode nodeId <> encode toEndPoint <> encode fromEndPoint 
encodeMessage (PONG nodeId toEndPoint) =
    encodeListLen 3 <> encodeWord 1 <> encode nodeId <> encode toEndPoint  
encodeMessage (FIND_NODE nodeId targetNodeId nodeEndPoint) = 
    encodeListLen 4 <> encodeWord 2 <> encode nodeId <> encode targetNodeId <> encode nodeEndPoint 
encodeMessage (FN_RESP nodeId peerList nodeEndPoint) = 
    encodeListLen 4 <> encodeWord 3 <> encode nodeId <> encode peerList <> encode nodeEndPoint 

decodeMessage :: Decoder s Message 
decodeMessage = do 
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of 
        (4,0) -> PING <$> decode <*> decode <*> decode 
        (3,1) -> PONG <$> decode <*> decode
        (4,2) -> FIND_NODE <$> decode <*> decode <*> decode 
        (4,3) -> FN_RESP <$> decode <*> decode <*> decode  
        _     -> fail "Invalid Message encoding"
