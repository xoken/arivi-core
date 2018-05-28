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
  , makeConnectionId
) where

import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Types                (ConnectionId, Event (..),
                                                     NodeId, OutboundFragment,
                                                     Parcel (..),
                                                     PersonalityType (..),
                                                     PortNumber, SequenceNum,
                                                     TransportType)
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.Logger
import qualified Crypto.PubKey.Curve25519           as Curve25519
import qualified Crypto.PubKey.Ed25519              as Ed25519
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, append, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import           GHC.Generics
import qualified Network.Socket                     as Network (HostAddress,
                                                                Socket)


data Connection = Connection {
                          connectionId          :: ConnectionId
                        , remoteNodeId          :: NodeId
                        , ipAddress             :: Network.HostAddress
                        , port                  :: PortNumber
                        , ephemeralPubKey       :: Curve25519.PublicKey
                        , ephemeralPrivKey      :: Ed25519.SecretKey
                        , transportType         :: TransportType
                        , personalityType       :: PersonalityType
                        , socket                :: Network.Socket
                        , sharedSecret          :: Keys.SharedSecret
                        , eventTChan            :: TChan Event
                        , outboundFragmentTChan :: TChan OutboundFragment
                        , reassemblyTChan       :: TChan Parcel
                        , egressSeqNum          :: SequenceNum
                        , ingressSeqNum         :: SequenceNum
                        -- , logChan               :: Chan (Loc, LogSource,
                        --                                  LogLevel, LogStr)
                        -- , timer                 :: Updatable
                        } deriving (Eq, Generic)

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
makeConnectionId :: Network.HostAddress
                 -> PortNumber
                 -> TransportType
                 -> ConnectionId
makeConnectionId ipAddress port transportType =
                         concatenate
                           (concatenate (concatenate ipAddress "|")
                             (concatenate port "|"))
                           (concatenate transportType "|")

