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
   , handleInboundConnection
) where

import           Arivi.Network.Connection as NetworkConnection
import qualified Arivi.Network.FSM        as FSM (initFSM)
import           Arivi.Network.Types
import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.STM   (atomically, newTChan, writeTChan)
import           Network.Socket

-- | Reads encryptedPayload and socket from inboundTChan, constructs
-- connectionId using `makeConnectionId`. If this connectionId is already
-- present in the frameDispatchHashMap  then reads parcelTChan from
-- frameDispatchHashMap and writes this encryptedPayload in it, otherwise
-- creates new parcelTChan and writes encryptedPayload to it and stores
-- this (connectionId,parcelTChan) in the frameDispatchHashMap

-- inboundConnectionHandler :: TChan (Socket)
--      -> HashMap ConnectionId (TChan ByteString)
--      -> IO (HashMap ConnectionId (TChan ByteString))
handleInboundConnection socket parcelTChan = do
        -- socket <- atomically $ readTChan inboundTChan

        socketName <- getSocketName socket

        let ipAddress = getIPAddress socketName
        let port = getPortNumber socketName
        let transportType = getTransportType socket

        let connectionId = NetworkConnection.makeConnectionId ipAddress
                                                                 port
                                                                 transportType

        -- if Data.HashMap.Strict.member connectionId frameDispatchHashMap
        --     then
        --       do
        --         let parcelTChan = fromJust (Data.HashMap.Strict.lookup
        --                                                 connectionId
        --                                                 frameDispatchHashMap)
        --         atomically $ writeTChan parcelTChan encryptedPayload
        --         inboundConnectionHandler inboundTChan frameDispatchHashMap
        -- else
        --     do
        -- parcelTChan <- atomically newTChan
        -- atomically $ writeTChan parcelTChan encryptedPayload
        connectionTChan <- atomically newTChan
        eventTChan <- atomically newTChan
        atomically $ writeTChan connectionTChan (socket,parcelTChan)
        -- serviceReqTChan <- atomically newTChan
        outboundTChan <- atomically newTChan
        reassemblyTChan <- atomically newTChan

        let connection = NetworkConnection.Connection
                                       connectionId undefined
                                       ipAddress port
                                       undefined undefined
                                       transportType undefined
                                       socket undefined
                                       -- serviceReqTChan parcelTChan
                                       eventTChan
                                       outboundTChan reassemblyTChan
                                       undefined undefined

        -- let updatedFrameDispatchHashMap = Data.HashMap.Strict.insert
        --                                   connectionId
        --                                   parcelTChan
        --                                   frameDispatchHashMap

        -- fsmHandle <- async (FSM.handleEvent connection FSM.Idle
        --                 (FSM.KeyExchangeInitEvent initParcel pvtKey))
        fsmHandle <- async (FSM.initFSM connection)

        -- async (readSock sock parcelTChan)
        --putStrLn ("listening on thread " ++  (show threadNo) )

        wait fsmHandle
        -- inboundConnectionHandler inboundTChan connectionTChan-- updatedFrameDispatchHashMap

-- | Given `SockAddr` retrieves `HostAddress`
getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet _ hostAddress) = hostAddress
getIPAddress _                            = error "getIPAddress: SockAddr is not of constructor SockAddrInet "

-- | Given `SockAddr` retrieves `PortNumber`
getPortNumber :: SockAddr -> PortNumber
getPortNumber (SockAddrInet portNumber _) = portNumber
getPortNumber _                           = error "getPortNumber: SockAddr is not of constructor SockAddrInet "

-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ Stream _ _) = TCP
getTransportType _                         = UDP
