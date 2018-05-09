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
    ConnectionId
  , P2PMessage(..)
  , Opcode(..)
  , ServiceCode
  , ServiceName
  , ServiceType(..)
  , ServiceRequest(..)
)

where

import           Arivi.Crypto.Utils.Keys.Encryption
import           Arivi.Kademlia.Types               (NodeId)
import           Arivi.Network.Types                (Payload (..))
import           Data.ByteString.Char8              (ByteString)
import           GHC.Generics



-- | These are the different types of Opcodes that are used identify different
--   types of events
data Opcode = ERROR  -- ^ If this Opcode is present in Message it contains Error
            | DATA   -- ^ If this Opcode is present in Message it is Normal
                     --   Message for data exchange
            | OFFER  -- ^ This Opcode shows the Message is Offer negotiation
                     --   Message
            | ANSWER -- ^ This Opcode shows that Message is answer for Offer
                     --   Message
            deriving (Show,Eq,Generic)

-- | ConnectionId is type synonym for ByteString
type ConnectionId = ByteString



-- | Message is the actual frame for the Arivi P2P Layer. Arivi Network Layer
--   will receive Parcel on wire, after removing the size field and decrypting
--   this Message will will given to the Arivi P2P Layer.
data P2PMessage   =  ServiceNegotiationOffer {
                    connectionId  :: ConnectionId    -- ^ It is concatenation of
                                                    --   IP Address, PortNumber
                                                    --   and TransportType
                ,   opcode        :: Opcode         -- ^ Denotes the opcode
                ,   fromNodeId    :: NodeId         -- ^ Sender of this Message
                ,   supportedList :: [(ServiceName,ServiceCode)]
                                                    -- ^ Tuple of `ServiceName`
                                                    --   and `ServiceCode`
                }

                | ServiceNegotiationResponse {
                   connectionId  :: ConnectionId   -- ^ It is concatenation of
                                                   --   IP Address, PortNumber
                                                   --   and TransportType
                ,  opcode        :: Opcode         -- ^ Denotes the opcode
                ,  fromNodeId    :: NodeId
                ,  supportedList :: [(ServiceName,ServiceCode)]
                                                    -- ^ Tuple of `ServiceName`
                                                    --   and `ServiceCode`
                }

               | DataMessage  {
                  connectionId :: ConnectionId    -- ^ It is concatenation of
                                                  --   IP Address, PortNumber
                                                  --   and TransportType
                , opcode       :: Opcode          -- ^ Denotes the opcode
                , service      :: ServiceCode     -- ^ Type of service
                                                  --   needed
                , payload      :: Payload         -- ^ Payload
               }

              | ErrorMessage  {
                  connectionId :: ConnectionId    -- ^ It is concatenation of
                                                  --   IP Address, PortNumber
                                                  --   and TransportType
                , opcode       :: Opcode          -- ^ Denotes the opcode
                , service      :: ServiceCode     -- ^ Type of service
                                                  --   needed
               }

               deriving (Show,Generic)



-- | ServiceCode shows the service Code like Block Syncing, Block Inventory,etc
type ServiceCode = ByteString

-- | ServiceName shows the service name like Block Syncing, Block Inventory,etc
type ServiceName = ByteString



-- | Indicates the ServiceType to execute
data ServiceType =  OPEN    -- ^ Used for Initiating Service Negotiation
                  | CLOSE   -- ^ Used for Terminating the connection
                  | SENDMSG -- ^ Used for Data Exchanges
                  deriving (Show,Eq)

-- | This is request contains the ServiceType and payload will be given to Arivi
--   Network Layer
data ServiceRequest =  SendMessageServiceRequest {

                           serviceType :: ServiceType  -- ^ Type of service
                                                       --   needed
                         , payloadData :: Payload      -- ^ Actual Payload
                       }
                      | OpenConnectionServiceRequest {

                           serviceType :: ServiceType  -- ^ Type of service
                                                       --   needed
                         , payloadData :: Payload      -- ^ Actual Payload
                       }

                       | CloseServiceRequest {
                           serviceType :: ServiceType  -- ^ Type of service
                       }
                       deriving (Show,Eq)
