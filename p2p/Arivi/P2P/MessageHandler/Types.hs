{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.MessageHandler.Types
(
  MessageCode,
  P2PMessage(..),
  Peer(..),
  IP,
  Port,
  P2PUUID,
  Message,
  UUIDMap,
  PeerUUIDMap
{-

-}
)
where


import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.MVar
import           Control.Monad

import           Codec.Serialise                (Serialise)

import           GHC.Generics                   (Generic)

import              Data.ByteString.Char8       as Char8 (ByteString, pack)
import              Data.UUID                   (UUID)
import              Data.UUID.V4                (nextRandom)
import              Data.HashMap.Strict         as HM
import              Data.Hashable

import           Arivi.Network.Types            (TransportType (..),NodeId)

--import Arivi.P2P.Types
type IP = String
type Port = Int
type P2PUUID = String
type MessageType = String -- for now
type Message = ByteString
data P2PMessage = P2PMessage {
          uuid :: P2PUUID
        , messageCode :: MessageCode
        , typeMessage :: Message
}deriving(Eq,Ord,Show,Generic)
instance Serialise P2PMessage
data MessageCode = Kademlia | RPC | PubSub deriving(Eq,Ord,Show,Generic)
instance Serialise MessageCode

data Peer =  Peer {
    peerNodeId :: NodeId
  , peerIp:: IP
  , peerUDPPort:: Port
  , peerTCPPort:: Port
} deriving(Eq,Ord,Show,Generic)

instance Hashable Peer
type UUIDMap = HM.HashMap P2PUUID (MVar P2PMessage)
type PeerUUIDMap = HM.HashMap Peer (TVar UUIDMap)

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