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
) where

import qualified Arivi.Network.Connection as NetworkConnection (Connection (..),
                                                                makeConnectionId)
import qualified Arivi.Network.FSM        as FSM (Event (KeyExchangeInitEvent),
                                                  State (Idle), handleEvent)
import           Arivi.Network.Types
import           Arivi.P2P.Types          (ServiceRequest (..),
                                           ServiceType (..))
import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.STM   (TChan, atomically, newTChan,
                                           readTChan, writeTChan)
import           Control.Monad            (forever)
import           Data.ByteString.Char8    (ByteString)
import           Data.HashMap.Strict      (HashMap, delete, empty, insert,
                                           lookup, member)
import           Data.Maybe
import           Network.Socket

-- | Reads encryptedPayload and socket from inboundTChan, constructs
-- connectionId using `makeConnectionId`. If this connectionId is already
-- present in the frameDispatchHashMap  then reads parcelCipherTChan from
-- frameDispatchHashMap and writes this encryptedPayload in it, otherwise
-- creates new parcelCipherTChan and writes encryptedPayload to it and stores
-- this (connectionId,parcelCipherTChan) in the frameDispatchHashMap

inboundFrameDispatcher :: TChan (ByteString, Socket)
     -> HashMap ConnectionId (TChan ByteString)
     -> IO (HashMap ConnectionId (TChan ByteString))
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
                return frameDispatchHashMap
        else
            do
                parcelCipherTChan <- atomically newTChan
                atomically $ writeTChan parcelCipherTChan encryptedPayload
                serviceReqTChan <- atomically newTChan

                let connection = NetworkConnection.Connection
                                               connectionId undefined
                                               ipAddress port
                                               undefined undefined
                                               transportType undefined
                                               socket undefined
                                               serviceReqTChan parcelCipherTChan
                                               undefined undefined

                let updatedFrameDispatchHashMap = Data.HashMap.Strict.insert
                                                  connectionId
                                                  parcelCipherTChan
                                                  frameDispatchHashMap

                fsmHandle <- async (FSM.handleEvent connection FSM.Idle
                                (FSM.KeyExchangeInitEvent encryptedPayload))
                wait fsmHandle
                return updatedFrameDispatchHashMap

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
