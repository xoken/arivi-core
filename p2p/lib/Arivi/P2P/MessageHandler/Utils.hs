{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Arivi.P2P.MessageHandler.Utils where

import           Arivi.Network                         (AriviNetworkException (..),
                                                        ConnectionHandle (..),
                                                        TransportType (..),
                                                        openConnection)
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar

import           Arivi.Utils.Logging
import           Control.Monad.Logger
import           Data.HashMap.Strict                   as HM
import           Data.String.Conv
import qualified Data.UUID                             as UUID (toString)
import           Data.UUID.V4                          (nextRandom)
import           Network.Socket                        (PortNumber)



logWithNodeId :: (HasLogging m) => NodeId -> String -> m ()
logWithNodeId peerNodeId logLine = $(logDebug) $ toS $ logLine ++ show peerNodeId

getNodeId :: TVar PeerDetails -> IO NodeId
getNodeId peerDetailsTVar =
    nodeId <$> readTVarIO peerDetailsTVar

-- | wraps the payload with message type { Kademlia | RPC | PubSub} and UUID
generateP2PMessage :: P2PUUID -> MessageType -> P2PPayload -> P2PMessage
generateP2PMessage = P2PMessage

insertToUUIDMap :: P2PUUID -> MVar P2PMessage -> PeerDetails -> PeerDetails
insertToUUIDMap uuid mvar peerDetails = peerDetails {uuidMap = HM.insert uuid mvar (uuidMap peerDetails)}

deleteFromUUIDMap :: P2PUUID -> PeerDetails -> PeerDetails
deleteFromUUIDMap uuid peerDetails = peerDetails {uuidMap = HM.delete uuid (uuidMap peerDetails)}

getHandlerByMessageType :: PeerDetails -> MessageType -> Handle
getHandlerByMessageType peerDetails RPC =  streamHandle peerDetails
getHandlerByMessageType peerDetails _   =  datagramHandle peerDetails

getTransportType :: MessageType -> TransportType
getTransportType msgType | msgType == RPC = TCP
                                      | otherwise = UDP

-- | Wrapper around openConnection
openConnectionToPeer :: (HasP2PEnv m, HasLogging m) => IP -> PortNumber -> TransportType -> NodeId -> m (Either AriviNetworkException ConnectionHandle)
openConnectionToPeer = openConnection

checkConnection :: PeerDetails -> TransportType -> Handle
checkConnection peerDetails TCP = streamHandle peerDetails
checkConnection peerDetails UDP = datagramHandle peerDetails

-- | Get a random unique id
getUUID :: IO P2PUUID
getUUID = UUID.toString <$> nextRandom

doesPeerExist :: TVar NodeIdPeerMap -> NodeId -> IO Bool
doesPeerExist nodeIdPeerTVar peerNodeId =
    HM.member peerNodeId <$> readTVarIO nodeIdPeerTVar

mkPeer :: NodeId -> IP -> PortNumber -> TransportType -> Handle -> STM (TVar PeerDetails)
mkPeer peerNodeId peerIp peerPort transportType connHandle = do
    lock <- newTMVar True
    tvar <- newTVar HM.empty
    let peerDetails = PeerDetails {
                    nodeId = peerNodeId
                , rep' = 0.0 -- needs to be a float. Not a Maybe Int
                , rep = Nothing
                , ip' = peerIp
                , ip = Nothing
                , udpPort' = peerPort
                , tcpPort' = peerPort
                , udpPort = Just peerPort
                , tcpPort = Just peerPort
                , streamHandle = case transportType of
                                    TCP -> connHandle
                                    UDP -> NotConnected
                , datagramHandle = case transportType of
                                        UDP -> connHandle
                                        TCP -> NotConnected
                , uuidMap = HM.empty
                , tvarUUIDMap = tvar
                , connectionLock = lock
            }
    newTVar peerDetails


-- | Adds new peer passed details
-- | Assume we get IP and portNumber as well.
-- | Need to discuss if its really needed to store IP and port in case that the node is the recipient of the handshake
addNewPeer ::
       NodeId
    -> TVar PeerDetails
    -> TVar NodeIdPeerMap
    -> STM ()
addNewPeer peerNodeId peerDetailsTVar nodeIdMapTVar =
    modifyTVar' nodeIdMapTVar (HM.insert peerNodeId peerDetailsTVar)

-- | Updates the NodeIdPeerMap with the passed details
-- | Need to discuss if its really needed to store IP and port in case that the node is the recipient of the handshake
updatePeer ::
       TransportType
    -> Handle
    -> TVar PeerDetails
    -> STM ()
updatePeer transportType connHandle peerDetailsTVar =
    modifyTVar' peerDetailsTVar updateConnHandle
    where
        updateConnHandle peerDetails =
            if transportType == TCP then
                peerDetails {streamHandle = connHandle}
            else peerDetails {datagramHandle = connHandle}
