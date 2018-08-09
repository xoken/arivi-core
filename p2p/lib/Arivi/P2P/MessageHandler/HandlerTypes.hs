{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, TypeSynonymInstances #-}

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
    , uuidMap
    , streamHandle
    , datagramHandle
    , connectionLock
    , rep
    ) where

import           Control.Concurrent.MVar

{-

-}
import           Arivi.Network                (ConnectionHandle (..),
                                               TransportType (..))
import           Arivi.P2P.Types
import           Codec.Serialise              (Serialise)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar  ()
import           Data.ByteString              as N (ByteString)
import           Data.ByteString.Lazy         as Lazy (ByteString)
import           Data.HashMap.Strict          as HM
import           GHC.Generics                 (Generic)
import           Network.Socket               (PortNumber)
import           Control.Lens.TH

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
--RpcTchan
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

type UUIDMap = HM.HashMap P2PUUID (MVar P2PMessage)

data PeerDetails = PeerDetails
    { _nodeId         :: NodeId
    , _rep           :: Double -- Can have a fixed default value
    , _ip            :: IP     -- Should always have a value
    , _udpPort       :: PortNumber
    , _tcpPort       :: PortNumber
    , _streamHandle   :: Handle
    , _datagramHandle :: Handle
    , _uuidMap        :: UUIDMap
    , _connectionLock :: TMVar Bool
    }

makeLensesWith classUnderscoreNoPrefixFields ''PeerDetails

type NodeIdPeerMap = HM.HashMap NodeId (TVar PeerDetails)

-- type ResourceDetails = (P2PUUID, NodeId)
type MessageTypeHandler m = P2PPayload -> m P2PPayload

type MessageTypeMap m = HM.HashMap MessageType (MessageTypeHandler m)
