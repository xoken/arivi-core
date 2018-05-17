-- |
-- Module      :  Arivi.Network.FrameDispatcher
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing dispatch of frames in
-- Arivi communication
module Arivi.Network.FrameDispatcher
(
     getIPAddress
   , getPortNumber
   , getTransportType
   , inboundFrameDispatcher
   , outboundFrameDispatcher
) where

import           Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg, encryptMsg)
import qualified Arivi.Network.Connection           as NetworkConnection (Connection (..),
                                                                          makeConnectionId)
import qualified Arivi.Network.FSM                  as FSM (Event (KeyExchangeInitEvent),
                                                            State (Idle),
                                                            handleEvent)
import           Arivi.Network.Stream
import           Arivi.Network.Types
import           Arivi.Network.Utils
import           Arivi.P2P.Types                    (ServiceRequest (..),
                                                     ServiceType (..))
import           Codec.Serialise
import           Control.Concurrent.Async           (async, wait)
import           Control.Concurrent.STM             (TChan, atomically,
                                                     newTChan, readTChan,
                                                     writeTChan)
import           Control.Monad                      (forever)
import qualified Data.Binary                        as Binary (decode, encode)
import qualified Data.ByteString.Char8              as B (ByteString, empty)
import qualified Data.ByteString.Lazy               as BSL
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, lookup, member)
import           Data.Int                           (Int16, Int64)
import           Data.Maybe
import           Network.Socket
-- | Reads encryptedPayload and socket from inboundTChan, constructs
-- connectionId using `makeConnectionId`. If this connectionId is already
-- present in the frameDispatchHashMap  then reads parcelCipherTChan from
-- frameDispatchHashMap and writes this encryptedPayload in it, otherwise
-- creates new parcelCipherTChan and writes encryptedPayload to it and stores
-- this (connectionId,parcelCipherTChan) in the frameDispatchHashMap

inboundFrameDispatcher :: TChan (B.ByteString, Socket)
     -> HashMap ConnectionId (TChan B.ByteString)
     -> IO (HashMap ConnectionId (TChan B.ByteString))
inboundFrameDispatcher inboundTChan frameDispatchHashMap = do
        (encryptedPayload,socket) <- atomically $ readTChan inboundTChan

        socketName <- getSocketName socket

        let ipAddress = getIPAddress socketName
        let port = getPortNumber socketName
        let transportType = getTransportType socket

        connectionId <- NetworkConnection.makeConnectionId ipAddress
                                                           port
                                                           transportType

        if Data.HashMap.Strict.member connectionId frameDispatchHashMap
            then
              do
                let parcelCipherTChan = fromJust (Data.HashMap.Strict.lookup
                                                        connectionId
                                                        frameDispatchHashMap)
                atomically $ writeTChan parcelCipherTChan encryptedPayload
                inboundFrameDispatcher inboundTChan frameDispatchHashMap
        else
            do
                parcelCipherTChan <- atomically newTChan
                atomically $ writeTChan parcelCipherTChan encryptedPayload
                serviceReqTChan <- atomically newTChan
                outboundTChan <- atomically newTChan

                let connection = NetworkConnection.Connection
                                               connectionId undefined
                                               ipAddress port
                                               undefined undefined
                                               transportType undefined
                                               socket undefined
                                               serviceReqTChan parcelCipherTChan
                                               outboundTChan undefined
                                               undefined

                let updatedFrameDispatchHashMap = Data.HashMap.Strict.insert
                                                  connectionId
                                                  parcelCipherTChan
                                                  frameDispatchHashMap

                fsmHandle <- async (FSM.handleEvent connection FSM.Idle
                                (FSM.KeyExchangeInitEvent encryptedPayload))
                wait fsmHandle
                inboundFrameDispatcher inboundTChan updatedFrameDispatchHashMap

-- | Given `SockAddr` retrieves `HostAddress`
getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet portNumber hostAddress) = hostAddress

-- | Given `SockAddr` retrieves `PortNumber`
getPortNumber :: SockAddr -> PortNumber
getPortNumber (SockAddrInet portNumber hostAddress) = portNumber

-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ transportType _ _) = if transportType == Stream
                                                      then
                                                        TCP
                                                    else
                                                        UDP



