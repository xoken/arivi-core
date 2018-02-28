-- This module defines some of the most fundamental data types that will be used 
-- throughout this kademlia implementation, there are sevelral advantages of 
-- this first being it enables the utilization of haskell's poweful type system 
-- and scond it makes code cleaner and structured.  

{-# LANGUAGE DeriveGeneric #-}
module Types 
  ( getTimeStamp,
    Message,
    NodeId,
    NodeAddress,
    Signature,
    Sequence,
    TimeStamp
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

newtype NodeId        = NodeId String deriving (Show,Generic)
newtype NodeAddress   = NodeAddress SockAddr deriving (Show,Generic) 
newtype Signature     = Signature String deriving (Show,Generic)
newtype Sequence      = Sequence Int deriving (Show,Generic)
newtype TimeStamp     = TimeStamp (UTCTime) deriving (Show,Generic)

-- Helper function to get timeStamp/ epoch 
getTimeStamp :: IO TimeStamp
getTimeStamp = do 
    tStamp <- getPOSIXTime 
    return $ TimeStamp (posixSecondsToUTCTime tStamp)


-- Custom data type to send & recieve message  
data Message = PING {
                    nodeId        :: NodeId
                ,   timeStamp     :: TimeStamp
                }
               |PONG {
                    nodeId        :: NodeId
                ,   timeStamp     :: TimeStamp 
               }
               |FIND_NODE { 
                    nodeId        :: NodeId
                ,   remoteNodeId  :: NodeId
                ,   nodeAddress   :: NodeAddress
                ,   seqNo         :: Sequence
                ,   signature     :: Signature
                ,   timeStamp     :: TimeStamp 
                }
               |FN_RESP {
                    nodeId        :: NodeId
                ,   peerList      :: [(NodeAddress,NodeId)] 
                ,   nodeAddress   :: NodeAddress 
                ,   seqNo         :: Sequence
                ,   signature     :: Signature
                ,   timeStamp     :: TimeStamp 
                }            
               deriving (Generic,Show)

-- Serialise instance of different custom types so that they can be encoded 
-- and decoded using serialise library which further allows these types 
-- to be serialised and thuse makes it possible to be sent across network 

instance Serialise NodeId 
instance Serialise NodeAddress 
instance Serialise Signature
instance Serialise Sequence
instance Serialise TimeStamp 

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
encodeMessage (PING nodeId timeStamp) = 
    encodeListLen 3 <> encodeWord 0 <> encode nodeId <> encode timeStamp
encodeMessage (PONG nodeId timeStamp) =
    encodeListLen 3 <> encodeWord 1 <> encode nodeId <> encode timeStamp   
encodeMessage (FIND_NODE nodeId findId address seqNo signature timeStamp) = 
    encodeListLen 6 <> encodeWord 2 <> encode nodeId <> encode findId <> encode address <> encode seqNo <> encode signature <> encode timeStamp
encodeMessage (FN_RESP nodeId knodes address seqNo signature timeStamp) = 
    encodeListLen 6 <> encodeWord 3 <> encode nodeId <> encode knodes <> encode address <> encode seqNo <> encode signature <> encode timeStamp

decodeMessage :: Decoder s Message 
decodeMessage = do 
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of 
        (3,0) -> PING <$> decode <*> decode 
        (3,1) -> PONG <$> decode <*> decode
        (6,2) -> FIND_NODE <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode
        (6,3) -> FN_RESP <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode 
        _     -> fail "Invalid Message encoding"