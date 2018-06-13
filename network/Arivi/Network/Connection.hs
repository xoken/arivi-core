{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Arivi.Network.Connection
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication

module Arivi.Network.Connection
(
    ConnectionId
  , Connection (..)
  , HandshakeStatus(..)
  , concatenate
  , genConnectionId
  , makeConnectionId
) where

import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Types                (AeadNonce, ConnectionId,
                                                     NodeId, Parcel (..),
                                                     PersonalityType (..),
                                                     PortNumber, SequenceNum,
                                                     TransportType)
import           Control.Concurrent.STM.TChan       (TChan)
import           Control.Concurrent.STM.TVar        (TVar)
import qualified Crypto.PubKey.Curve25519           as Curve25519
import qualified Crypto.PubKey.Ed25519              as Ed25519
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, append, pack)
import qualified Data.ByteString.Lazy               as L
import           GHC.Generics
import qualified Network.Socket                     as Network (HostName,
                                                                SockAddr,
                                                                Socket)

data Connection = Connection {
                          connectionId      :: ConnectionId
                        , remoteNodeId      :: NodeId
                        , ipAddress         :: Network.HostName
                        , port              :: PortNumber
                        , ephemeralPubKey   :: Curve25519.PublicKey
                        , ephemeralPrivKey  :: Ed25519.SecretKey
                        , transportType     :: TransportType
                        , personalityType   :: PersonalityType
                        , socket            :: Network.Socket
                        , sharedSecret      :: Keys.SharedSecret
                        , remoteSockAddr    :: Network.SockAddr
                        , reassemblyTChan   :: TChan Parcel
                        , hsBufferTChan     :: TChan Parcel
                        , p2pMessageTChan   :: TChan L.ByteString
                        , egressSeqNum      :: TVar SequenceNum
                        , ingressSeqNum     :: TVar SequenceNum-- need not be TVar
                        , aeadNonceCounter  :: TVar AeadNonce
                        , handshakeComplete :: TVar HandshakeStatus
                        } deriving (Eq, Generic)

data HandshakeStatus = HandshakeNotStarted
                     | HandshakeInitiated
                     | HandshakeDone
                     deriving (Eq,Generic,Show)
-- | Generates a random 4 Byte ConnectionId using Raaz's random ByteString
-- generation
genConnectionId :: IO ByteString
genConnectionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Takes two arguments converts them into ByteString and concatenates them
concatenate :: (Show first, Show second) => first -> second -> ByteString
concatenate first second = Data.ByteString.Char8.append
                           (Data.ByteString.Char8.pack $ show first)
                           (Data.ByteString.Char8.pack $ show second)


-- | ConnectionId is concatenation of IP Address, PortNumber and TransportType
makeConnectionId :: Network.HostName
                 -> PortNumber
                 -> TransportType
                 -> ConnectionId
makeConnectionId mIpAddress mPort mTransportType =

                          Data.ByteString.Char8.pack $   mIpAddress
                                                     ++ "|"
                                                     ++ show mPort
                                                     ++ "|"
                                                     ++ show mTransportType

