{-# LANGUAGE DeriveGeneric #-}
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
(
--    ConnectionId
--   ,P2PMessage(..)
--   ,Opcode(..)
--   ,ServiceCode
--   ,ServiceName
--   ,ServiceType(..)
--   ,ServiceRequest(..)

        NodeType(..)
      , ServiceCode(..)
      , TopicCode(..)
      , AriviP2PInstance(..)
      , IP
      , Port
      , ExpiryTime
      , PeerList
      , MinPeerCountPerTopic
      , Context
      , TopicContext
      , TopicToService
      , SubscriptionMap
      , WatchersMap
      , MessageType(..)
      , P2PMessage(..)
      , Peer(..)
      , ResourceID
      , ResponseCode
      , P2PUUID
      , ResourceDetails
      , ResourceList
)

where

import qualified Data.Map                   as Map
import           Arivi.Network.Types        (TransportType (..),NodeId)
import           GHC.Generics               (Generic)
import           Codec.Serialise

type IP = String
type Port = Int
type ExpiryTime = Float
type PeerList = [ (NodeId,IP,Port,ExpiryTime) ]
type MinPeerCountPerTopic = Int
type Context = (MinPeerCountPerTopic, NodeType, TransportType)

type TopicContext  = Map.Map TopicCode Context
type TopicToService  = Map.Map TopicCode ServiceCode
type SubscriptionMap = Map.Map TopicCode PeerList
type WatchersMap = Map.Map TopicCode PeerList
type TopicToServiceMap = Map.Map TopicCode ServiceCode

-- need to update *starts*
type ResourceID = String
type ResponseCode = Int
type P2PUUID = String

type RPCPeerList = [ Peer ]
type ResourceDetails = (ServiceCode, RPCPeerList)
type ResourceList = Map.Map ResourceID ResourceDetails
-- need to update *ends*


data NodeType = FullNode | HalfNode deriving(Show)

data ServiceCode = BlockInventory | BlockSync | PendingTransaction 
                        deriving(Eq,Ord,Show)

data TopicCode = Latest_block_header | Latest_block
                    deriving(Eq,Ord,Show,Generic)
instance Serialise TopicCode

data AriviP2PInstance = AriviP2PInstance { nodeId :: NodeId
                                         , ip:: String
                                         , port:: Int
                                         , outboundPeerQuota:: Float
                                         , maxConnectionAllowed:: Int
                        }


data Peer =  Peer { 
    peerNodeId :: NodeId 
  , peerIp:: String
  , peerPort:: Int
  , peerTransportType :: TransportType
}

data MessageType =
      Subscribe {
          expires :: ExpiryTime
        , topic   :: TopicCode
    }
    | Notify {
          topic   :: TopicCode
        , serviceMessage :: String
    }
    | Publish {
          topic   :: TopicCode
        , serviceMessage :: String
    }
    | Options
    | Supported {
          resourceList :: [ResourceID]
    }
    | Get {
          resource :: ResourceID
        , serviceMessage :: String
    }
    | Return {
          resource :: ResourceID 
        , serviceMessage :: String
    }
    | Response {
          expires :: ExpiryTime
        , topic   :: TopicCode
    }
    
data P2PMessage = P2PMessage {
          uuid :: P2PUUID
        , to :: NodeId
        , from :: NodeId
        , responseCode :: ResponseCode
        , p2pType :: Maybe MessageType
}

-- =================================================================================================
-- import           Arivi.Crypto.Utils.Keys.Encryption
-- import           Arivi.Kademlia.Types               (NodeId)
-- import           Arivi.Network.Types                (Payload (..))
-- import           Data.ByteString.Char8              (ByteString)
-- import           GHC.Generics



-- -- | These are the different types of Opcodes that are used identify different
-- --   types of events
-- data Opcode = ERROR  -- ^ If this Opcode is present in Message it contains Error
--             | DATA   -- ^ If this Opcode is present in Message it is Normal
--                      --   Message for data exchange
--             | OFFER  -- ^ This Opcode shows the Message is Offer negotiation
--                      --   Message
--             | ANSWER -- ^ This Opcode shows that Message is answer for Offer
--                      --   Message
--             deriving (Show,Eq,Generic)

-- -- | ConnectionId is type synonym for ByteString
-- type ConnectionId = ByteString



-- -- | Message is the actual frame for the Arivi P2P Layer. Arivi Network Layer
-- --   will receive Parcel on wire, after removing the size field and decrypting
-- --   this Message will will given to the Arivi P2P Layer.
-- data P2PMessage   =  ServiceNegotiationOffer {
--                     connectionId  :: ConnectionId    -- ^ It is concatenation of
--                                                     --   IP Address, PortNumber
--                                                     --   and TransportType
--                 ,   opcode        :: Opcode         -- ^ Denotes the opcode
--                 ,   fromNodeId    :: NodeId         -- ^ Sender of this Message
--                 ,   supportedList :: [(ServiceName,ServiceCode)]
--                                                     -- ^ Tuple of `ServiceName`
--                                                     --   and `ServiceCode`
--                 }

--                 | ServiceNegotiationResponse {
--                    connectionId  :: ConnectionId   -- ^ It is concatenation of
--                                                    --   IP Address, PortNumber
--                                                    --   and TransportType
--                 ,  opcode        :: Opcode         -- ^ Denotes the opcode
--                 ,  fromNodeId    :: NodeId
--                 ,  supportedList :: [(ServiceName,ServiceCode)]
--                                                     -- ^ Tuple of `ServiceName`
--                                                     --   and `ServiceCode`
--                 }

--                | DataMessage  {
--                   connectionId :: ConnectionId    -- ^ It is concatenation of
--                                                   --   IP Address, PortNumber
--                                                   --   and TransportType
--                 , opcode       :: Opcode          -- ^ Denotes the opcode
--                 , service      :: ServiceCode     -- ^ Type of service
--                                                   --   needed
--                 , payload      :: Payload         -- ^ Payload
--                }

--               | ErrorMessage  {
--                   connectionId :: ConnectionId    -- ^ It is concatenation of
--                                                   --   IP Address, PortNumber
--                                                   --   and TransportType
--                 , opcode       :: Opcode          -- ^ Denotes the opcode
--                 , service      :: ServiceCode     -- ^ Type of service
--                                                   --   needed
--                }

--                deriving (Show,Generic)



-- -- | ServiceCode shows the service Code like Block Syncing, Block Inventory,etc
-- type ServiceCode = ByteString

-- -- | ServiceName shows the service name like Block Syncing, Block Inventory,etc
-- type ServiceName = ByteString



-- -- | Indicates the ServiceType to execute
-- data ServiceType =  OPEN    -- ^ Used for Initiating Service Negotiation
--                   | CLOSE   -- ^ Used for Terminating the connection
--                   | SENDMSG -- ^ Used for Data Exchanges
--                   | HANDSHAKE_TIMEOUT -- ^ Used for Data TimeOuts for events
--                   | DATA_TIMEOUT -- ^ Used for Data TimeOuts for events
--                   | PING_TIMEOUT -- ^ Used for Data TimeOuts for events
--                   deriving (Show,Eq)

-- -- | This is request contains the ServiceType and payload will be given to Arivi
-- --   Network Layer
-- data ServiceRequest =  SendMessageServiceRequest {

--                            serviceType :: ServiceType  -- ^ Type of service
--                                                        --   needed
--                          , payloadData :: Payload      -- ^ Actual Payload
--                        }
--                       | OpenConnectionServiceRequest {

--                            serviceType :: ServiceType  -- ^ Type of service
--                                                        --   needed
--                          , secretKey   :: SecretKey    -- ^ Actual Payload
--                        }

--                        | CloseServiceRequest {
--                            serviceType :: ServiceType  -- ^ Type of service
--                        }
--                        | TimeOutServiceRequest {
--                            serviceType :: ServiceType  -- ^ Type of service
--                        }
--                        deriving (Show,Eq)