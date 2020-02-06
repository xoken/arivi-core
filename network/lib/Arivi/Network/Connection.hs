{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}

--
-- This module provides useful functions for managing connections in Arivi
-- communication
module Arivi.Network.Connection
    ( ConnectionId
    , CompleteConnection
    , HandshakeStatus(..)
    , IncompleteConnection
    , aeadNonceCounter
    , concatenate
    , connectionId
    , egressSeqNum
    , genConnectionId
    , handshakeComplete
    , inboundDatagramTChan
    , ingressSeqNum
    , ipAddress
    , makeConnectionId
    , mkCompleteConnection
    , mkIncompleteConnection
    , mkIncompleteConnection'
    , p2pMessageTChan
    , personalityType
    , port
    , remoteNodeId
    , remoteSockAddr
    , sharedSecret
    , socket
    , transportType
    , waitWrite
    , pendingList
    ) where

import Arivi.Crypto.Utils.Keys.Encryption as Keys
import Arivi.Crypto.Utils.Random
import Arivi.Network.Types
    ( AeadNonce
    , ConnectionId
    , NodeId
    , Parcel(..)
    , PersonalityType(..)
    , PortNumber
    , SequenceNum
    , TransportType
    )
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, append, pack)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import qualified Network.Socket as Network (HostName, SockAddr, Socket)

data HandshakeStatus
    = HandshakeNotStarted
    | HandshakeInitiated
    | HandshakeDone
    deriving (Eq, Generic, Show)

data Connection a =
    Connection
        { connectionId :: ConnectionId
        , remoteNodeId :: NodeId
        , ipAddress :: Network.HostName
        , port :: PortNumber
        , transportType :: TransportType
        , personalityType :: PersonalityType
        , socket :: Network.Socket
        , waitWrite :: MVar Int
        , _cSharedSecret :: Keys.SharedSecret
        , remoteSockAddr :: Network.SockAddr
        , inboundDatagramTChan :: TChan Parcel
        , p2pMessageTChan :: TChan L.ByteString
        , egressSeqNum :: TVar SequenceNum
        , ingressSeqNum :: TVar SequenceNum -- need not be TVar
        , aeadNonceCounter :: TVar AeadNonce
        , handshakeComplete :: TVar HandshakeStatus
        , pendingList :: TVar [(Integer, Integer)]
        }
    deriving (Eq, Generic)

data Complete

data Incomplete

type IncompleteConnection = Connection Incomplete

type CompleteConnection = Connection Complete

mkIncompleteConnection ::
       ConnectionId
    -> NodeId
    -> Network.HostName
    -> PortNumber
    -> TransportType
    -> PersonalityType
    -> Network.Socket
    -> AeadNonce
    -> IO IncompleteConnection
mkIncompleteConnection cid rnid host portNum tt pt sock nonce = do
    msgChan <- newTChanIO
    egressNum <- newTVarIO 0
    ingressNum <- newTVarIO 0
    aeadNonce <- newTVarIO nonce
    writeLock <- newMVar 0
    mPendingList <- newTVarIO [(0, (2 :: Integer) ^ (32 :: Integer))]
    return
        Connection
            { connectionId = cid
            , remoteNodeId = rnid
            , ipAddress = host
            , port = portNum
            , transportType = tt
            , personalityType = pt
            , socket = sock
            , waitWrite = writeLock
            , p2pMessageTChan = msgChan
            , egressSeqNum = egressNum
            , ingressSeqNum = ingressNum
            , aeadNonceCounter = aeadNonce
            , pendingList = mPendingList
            }

mkIncompleteConnection' :: Connection Incomplete
mkIncompleteConnection' = Connection {}

mkCompleteConnection :: Connection Incomplete -> Keys.SharedSecret -> CompleteConnection
mkCompleteConnection connection ssk = connection {_cSharedSecret = ssk}

sharedSecret :: Connection Complete -> Keys.SharedSecret
sharedSecret = _cSharedSecret

-- | Generates a random 4 Byte ConnectionId using Raaz's random ByteString
-- generation
genConnectionId :: IO ByteString
genConnectionId = getRandomByteString 4 >>= \byteString -> return (encode byteString)

-- | Takes two arguments converts them into ByteString and concatenates them
concatenate :: (Show first, Show second) => first -> second -> ByteString
concatenate first second =
    Data.ByteString.Char8.append (Data.ByteString.Char8.pack $ show first) (Data.ByteString.Char8.pack $ show second)

-- | ConnectionId is concatenation of IP Address, PortNumber and TransportType
makeConnectionId :: Network.HostName -> PortNumber -> TransportType -> ConnectionId
makeConnectionId mIpAddress mPort mTransportType =
    Data.ByteString.Char8.pack $ mIpAddress ++ "|" ++ show mPort ++ "|" ++ show mTransportType
