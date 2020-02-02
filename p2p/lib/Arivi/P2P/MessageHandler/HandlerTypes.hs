{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleInstances, TypeSynonymInstances #-}

module Arivi.P2P.MessageHandler.HandlerTypes
    ( MessageType(..)
    , P2PMessage(..)
    , PeerDetails(..)
    , defaultPeerDetails
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
    , HasNetworkConfig(..)
    , MessageTypeMap
    , MessageTypeHandler
    , uuidMap
    , streamHandle
    , datagramHandle
    , connectionLock
    , rep
    ) where

import Arivi.Network (ConnectionHandle(..), TransportType(..))
import Arivi.P2P.Types
import Codec.Serialise (Serialise)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens.TH
import Data.ByteString as N (ByteString)
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import Network.Socket (PortNumber)

type IP = String

type Port = PortNumber

type NodeId = N.ByteString

type P2PUUID = String

type P2PPayload = Lazy.ByteString

data P2PMessage =
    P2PMessage
        { uuid :: Maybe P2PUUID
        , messageType :: MessageType
        , payload :: P2PPayload
        }
    deriving (Eq, Ord, Show, Generic)

instance Serialise P2PMessage

type MessageInfo = (P2PUUID, P2PPayload)

data Handle
    = NotConnected
    | Connected ConnectionHandle

instance Eq Handle where
    NotConnected == NotConnected = True
    Connected _ == Connected _ = True
    _ == _ = False

type UUIDMap = HM.HashMap P2PUUID (MVar P2PMessage)

data PeerDetails =
    PeerDetails
        { _networkConfig :: NetworkConfig
        , _rep :: Double -- Can have a fixed default value
        , _streamHandle :: Handle
        , _datagramHandle :: Handle
        , _uuidMap :: UUIDMap
        , _connectionLock :: TMVar Bool
        }

defaultPeerDetails :: STM PeerDetails
defaultPeerDetails = do
    lock <- newTMVar False
    return
        PeerDetails
            { _networkConfig = defaultNetworkConfig
            , _rep = 0.0
            , _streamHandle = NotConnected
            , _datagramHandle = NotConnected
            , _uuidMap = HM.empty
            , _connectionLock = lock
            }

makeLensesWith classUnderscoreNoPrefixFields ''PeerDetails

type NodeIdPeerMap = HM.HashMap NodeId (TVar PeerDetails)

-- type ResourceDetails = (P2PUUID, NodeId)
type MessageTypeHandler m = P2PPayload -> m P2PPayload

type MessageTypeMap m = HM.HashMap MessageType (MessageTypeHandler m)
