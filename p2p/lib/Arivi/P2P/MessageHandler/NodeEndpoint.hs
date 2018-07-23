{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Arivi.P2P.MessageHandler.NodeEndpoint (
      sendRequest
    , receiveResponse
    , newIncomingConnectionHandler
) where

import           Arivi.Network                        (ConnectionHandle (..),
                                                       TransportType (..),
                                                       openConnection)
import           Arivi.P2.MessageHandler.HandlerTypes
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import Data.HashMap.Strict as HM

-- | Adds or Updates the NodeIdPeerMap with the passed details
-- | Assume we get IP and portNumber as well.
-- | Need to discuss if its really needed to store IP and port in case that the node is the recipient of the handshake
addPeer ::
       NodeId
    -> IP
    -> PortNumber
    -> TransportType
    -> ConnectionHandle
    -> TVar NodeIdPeerMap
    -> STM ()
addPeer peerNodeId peerIp portNum transportType connHandle tvar =
    -- Strict modify
    modifyTVar' tvar func 
    where
        func nodeIdPeerMap = do
            case HM.lookup peerNodeId nodeIdPeerMap of
                Nothing -> do
                    -- create a new PeerDetails record
                    let peerDetails = PeerDetails {
                          nodeId = peerNodeId
                        , rep = 0 -- needs to be a float. Not a Maybe Int
                        , ip = Just peerIp
                        , udpPort = case transportType of 
                                        UDP -> Just portNum
                                        _   -> Nothing
                        , tcpPort = case transportType of 
                                        TCP -> Just portNum
                                        _   -> Nothing
                        , streamHandle = case transportType of
                                            TCP -> Connected {connId = connHandle}
                                            UDP -> Nothing 
                        , datagramHandle = case transportType of
                                                UDP -> Connected {connId = connHandle}
                                                TCP -> Nothing
                        , tvarUUIDMap = newTVar HM.empty
                    }
                Just peerDetails -> do
                    let updatedPeerDetails = 
                        case transportType of 
                            TCP -> peerDetails {streamHandle = } 

newIncomingConnectionHandler :: (HasP2PEnv m, HasLogging m)
    => NodeId
    -> TransportType
    -> ConnectionHandle
    -> m ()
newIncomingConnectionHandler nodeId transportType connHandle = do
    return ()
