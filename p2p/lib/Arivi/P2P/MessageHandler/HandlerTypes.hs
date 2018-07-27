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
    , ConnectionHandle(..)
    , TransportType(..)
    , NodeIdPeerMap
    , Handle(..)
    , MessageTypeMap
    , MessageTypeHandler
    ) where

import           Control.Concurrent.MVar

{-

-}
import           Arivi.Network                (ConnectionHandle (..),
                                               TransportType (..))
import           Codec.Serialise              (Serialise)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar  ()
import           Data.ByteString              as N (ByteString)
import           Data.ByteString.Lazy         as Lazy (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict          as HM
import           GHC.Generics                 (Generic)
import           Network.Socket               (PortNumber)

--import           Arivi.Network.Types            (TransportType(..))
--import Arivi.P2P.Types
type IP = String

type Port = PortNumber

type NodeId = N.ByteString

type P2PUUID = String

type P2PPayload = Lazy.ByteString

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
    | Connected ConnectionHandle

instance Eq Handle where
    NotConnected == NotConnected = True
    Pending == Pending = True
    Connected _ == Connected _ = True
    _ == _ = False

data PeerDetails = PeerDetails
    { nodeId         :: NodeId
    , rep            :: Maybe Int -- Can have a fixed default value
    , ip             :: Maybe IP     -- Should always have a value
    , rep'           :: Double -- Can have a fixed default value
    , ip'            :: IP     -- Should always have a value
    , udpPort'       :: PortNumber
    , udpPort        :: Maybe PortNumber
    , tcpPort'       :: PortNumber
    , tcpPort        :: Maybe PortNumber
    , streamHandle   :: Handle
    , datagramHandle :: Handle
    , uuidMap        :: UUIDMap
    , tvarUUIDMap    :: TVar UUIDMap
    , connectionLock :: TMVar Bool
    }

type UUIDMap = HM.HashMap P2PUUID (MVar P2PMessage)

type NodeIdPeerMap = HM.HashMap NodeId (TVar PeerDetails)

-- type ResourceDetails = (P2PUUID, NodeId)
type MessageTypeHandler m = P2PPayload -> m P2PPayload

type MessageTypeMap m = HM.HashMap MessageType (MessageTypeHandler m)
