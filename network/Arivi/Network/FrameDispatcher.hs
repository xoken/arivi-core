-- {-# OPTIONS_GHC -fno-warn-missing-fields #-}
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

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection        as Conn
import qualified Arivi.Network.FSM               as FSM (initFSM)
import           Arivi.Network.Types
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.STM          (TChan, atomically, newTChan)
import           Control.Monad.IO.Class
import           Debug.Trace
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
handleInboundConnection :: (HasSecretKey m, HasLogging m) => Socket -> TChan Event -> m ()
handleInboundConnection mSocket mEventTChan = do
        -- mSocket <- atomically $ readTChan inboundTChan

        conn <- liftIO $ do
          socketName <- getSocketName mSocket
          mIpAddress <- inet_ntoa $ getIPAddress socketName
          traceShow (show socketName ++ "frameDispatchHashMap") (return())
          let mPort = getPortNumber socketName
          let mTransportType = getTransportType mSocket
          let mConnectionId = Conn.makeConnectionId mIpAddress mPort mTransportType

        -- if Data.HashMap.Strict.member connectionId frameDispatchHashMap
        --     then
        --       do
        --         let eventTChan = fromJust (Data.HashMap.Strict.lookup
        --                                                 connectionId
        --                                                 frameDispatchHashMap)
        --         atomically $ writeTChan eventTChan encryptedPayload
        --         inboundConnectionHandler inboundTChan frameDispatchHashMap
        -- else
        --     do
        -- eventTChan <- atomically newTChan
        -- atomically $ writeTChan eventTChan encryptedPayload
        -- serviceReqTChan <- atomically newTChan
          outboundTChan <- atomically newTChan
          mReassemblyTChan <- atomically newTChan
          p2pMsgTChan <- atomically newTChan
          let connection = Conn.Connection { Conn.connectionId = mConnectionId
                                           , Conn.ipAddress = mIpAddress
                                           , Conn.port = mPort
                                           , Conn.transportType = mTransportType
                                           , Conn.personalityType = RECIPIENT
                                           , Conn.socket = mSocket
                                           , Conn.eventTChan = mEventTChan
                                           , Conn.outboundFragmentTChan = outboundTChan
                                           , Conn.reassemblyTChan = mReassemblyTChan
                                           , Conn.p2pMessageTChan = p2pMsgTChan
                                           }
          return connection

        -- let updatedFrameDispatchHashMap = Data.HashMap.Strict.insert
        --                                   connectionId
        --                                   eventTChan
        --                                   frameDispatchHashMap

        -- fsmHandle <- async (FSM.handleEvent connection FSM.Idle
        --                 (FSM.KeyExchangeInitEvent initParcel pvtKey))
        -- fsmHandle <- async (FSM.initFSM conn)
        _ <- async (FSM.initFSM conn)
        -- fsmHandle <- async (undefined)
        -- async (readSock sock eventTChan)
        --putStrLn ("listening on thread " ++  (show threadNo) )
        return ()
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
