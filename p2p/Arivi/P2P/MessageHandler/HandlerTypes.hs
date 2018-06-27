{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}

module Arivi.P2P.MessageHandler.HandlerTypes
    ( MessageType(..)
    , P2PMessage(..)
    , PeerDetails(..)
    , IP
    , Port
    , P2PUUID
    , P2PPayload
    , UUIDMap
    , MessageInfo
    , NodeId
    , ConnectionId
    , TransportType(..)
    , NodeIdPeerMap
    , Handle(..)
    , MessageTypeMap
    , MessageTypeHandler
    ) where

import           Control.Concurrent.MVar

{-

-}
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar ()
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO)

import           Codec.Serialise             (Serialise)

import           GHC.Generics                (Generic)

import           Data.ByteString.Char8       as Char8 (ByteString)

import           Data.Hashable
import           Data.HashMap.Strict         as HM
import           Network.Socket              (PortNumber)

--import           Arivi.Network.Types            (TransportType(..))
--import Arivi.P2P.Types
type IP = String

type Port = Int

type NodeId = ByteString

type P2PUUID = String

type P2PPayload = ByteString

type ConnectionId = ByteString

data P2PMessage = P2PMessage
    { uuid        :: P2PUUID
    , messageType :: MessageType
    , payload     :: P2PPayload
    } deriving (Eq, Ord, Show, Generic)

instance Serialise P2PMessage

data MessageType
    = Kademlia
    | RPC
    | PubSub
    | Option
    deriving (Eq, Ord, Show, Generic)

instance Serialise MessageType

instance Hashable MessageType

-- data Peer = Peer
--     { nodeId  :: NodeId
--     , ip      :: IP
--     , udpPort :: Port
--     , tcpPort :: Port
--     } deriving (Eq, Show, Generic)
data TransportType
    = UDP
    | TCP
    deriving (Eq, Show, Generic, Read)

instance Hashable TransportType

{-
PeerToUUIDMap =
TVar( HashMap[ NodeId->TVar( HashMap[ UUID->MVar] ) ] )
--used for storing blocked readMVars for each sent request

UUID generation done here

P2PMessage = {
    UUID
    To
    From
    MessageType --sent by caller
}

--final message to be sent to network layer

--KademTchan
--RPCTchan
--PubSubTchan
-}
type MessageInfo = (P2PUUID, P2PPayload)

data Handle
    = NotConnected
    | Pending
    | Connected { connId :: ConnectionId }
    deriving (Eq, Ord, Show, Generic)

data PeerDetails = PeerDetails
    { nodeId         :: NodeId
    , rep            :: Maybe Int
    , ip             :: Maybe IP
    , udpPort        :: Maybe PortNumber
    , tcpPort        :: Maybe PortNumber
    , streamHandle   :: Handle
    , datagramHandle :: Handle
    , tvarUUIDMap    :: TVar UUIDMap
    }

type UUIDMap = HM.HashMap P2PUUID (MVar P2PMessage)

type NodeIdPeerMap = HM.HashMap NodeId (TVar PeerDetails)

-- type ResourceDetails = (P2PUUID, NodeId)
type MessageTypeHandler m = P2PPayload -> m P2PPayload

type MessageTypeMap m = HM.HashMap MessageType (MessageTypeHandler m)
