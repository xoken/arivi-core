{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arivi.P2P.MessageHandler.Utils
    ( module Arivi.P2P.MessageHandler.Utils
    ) where

import Arivi.Network (AriviNetworkException(..), ConnectionHandle(..), TransportType(..), openConnection)
import Arivi.P2P.Exception
import Arivi.P2P.MessageHandler.HandlerTypes hiding (uuid)
import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Arivi.Utils.Logging

import Codec.Serialise (DeserialiseFailure)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Logger
import Data.HashMap.Strict as HM
import Data.String.Conv
import qualified Data.UUID as UUID (toString)
import Data.UUID.V4 (nextRandom)

logWithNodeId :: (HasLogging m) => NodeId -> String -> m ()
logWithNodeId peerNodeId logLine = $(logDebug) $ toS $ logLine ++ show peerNodeId

getNodeId :: TVar PeerDetails -> IO NodeId
getNodeId peerDetailsTVar = (^. networkConfig . nodeId) <$> readTVarIO peerDetailsTVar

-- | wraps the payload with message type { Kademlia | Rpc | PubSub} and UUID
generateP2PMessage :: Maybe P2PUUID -> MessageType -> P2PPayload -> P2PMessage
generateP2PMessage = P2PMessage

insertToUUIDMap :: P2PUUID -> MVar P2PMessage -> PeerDetails -> PeerDetails
insertToUUIDMap uuid mvar peerDetails = peerDetails & uuidMap . at uuid ?~ mvar

deleteFromUUIDMap :: P2PUUID -> PeerDetails -> PeerDetails
deleteFromUUIDMap uuid peerDetails = peerDetails & uuidMap %~ HM.delete uuid

getHandlerByMessageType :: PeerDetails -> TransportType -> Handle
getHandlerByMessageType peerDetails TCP = peerDetails ^. streamHandle
getHandlerByMessageType peerDetails UDP = peerDetails ^. datagramHandle

getTransportType :: MessageType -> TransportType
getTransportType Kademlia = UDP
getTransportType Option = TCP
getTransportType _ = TCP

networkToP2PException :: Either AriviNetworkException a -> Either AriviP2PException a
networkToP2PException (Left e) = Left (NetworkException e)
networkToP2PException (Right a) = Right a

-- | Wrapper around openConnection
openConnectionToPeer ::
       (HasNodeEndpoint m, HasLogging m)
    => NetworkConfig
    -> TransportType
    -> m (Either AriviNetworkException ConnectionHandle)
openConnectionToPeer = openConnection

safeDeserialise :: Either DeserialiseFailure a -> Either AriviP2PException a
safeDeserialise (Left _) = Left DeserialiseFailureP2P
safeDeserialise (Right a) = Right a

checkConnection :: PeerDetails -> TransportType -> Handle
checkConnection peerDetails TCP = peerDetails ^. streamHandle
checkConnection peerDetails UDP = peerDetails ^. datagramHandle

-- | Get a random unique id
getUUID :: IO P2PUUID
getUUID = UUID.toString <$> nextRandom

doesPeerExist :: TVar NodeIdPeerMap -> NodeId -> IO Bool
doesPeerExist nodeIdPeerTVar peerNodeId = HM.member peerNodeId <$> readTVarIO nodeIdPeerTVar

mkPeer :: NetworkConfig -> TransportType -> Handle -> STM (TVar PeerDetails)
mkPeer nc transportType connHandle = do
    lock <- newTMVar True
    let peerDetails =
            PeerDetails
                { _networkConfig = nc
                , _rep = 0.0 -- needs to be a float. Not a Maybe Int
                , _streamHandle =
                      case transportType of
                          TCP -> connHandle
                          UDP -> NotConnected
                , _datagramHandle =
                      case transportType of
                          UDP -> connHandle
                          TCP -> NotConnected
                , _uuidMap = HM.empty
                , _connectionLock = lock
                }
    newTVar peerDetails

-- | Adds new peer passed details
-- | Assume we get IP and portNumber as well.
-- | Need to discuss if its really needed to store IP and port in case that the node is the recipient of the handshake
addNewPeer :: NodeId -> TVar PeerDetails -> TVar NodeIdPeerMap -> STM ()
addNewPeer peerNodeId peerDetailsTVar nodeIdMapTVar = modifyTVar' nodeIdMapTVar (HM.insert peerNodeId peerDetailsTVar)

-- | Updates the NodeIdPeerMap with the passed details
-- | Need to discuss if its really needed to store IP and port in case that the node is the recipient of the handshake
updatePeer :: TransportType -> Handle -> TVar PeerDetails -> STM ()
updatePeer transportType connHandle peerDetailsTVar = modifyTVar' peerDetailsTVar updateConnHandle
  where
    updateConnHandle peerDetails =
        if transportType == TCP
            then peerDetails & streamHandle .~ connHandle
            else peerDetails & datagramHandle .~ connHandle

-- | Create an entry in the nodeIdPeerDetails map with the given NetworkConfig and transportType
addPeerToMap :: NetworkConfig -> TransportType -> TVar NodeIdPeerMap -> STM ()
addPeerToMap nc transportType nodeIdMapTVar = do
    peerDetailsTVar <- mkPeer nc transportType NotConnected
    addNewPeer (nc ^. nodeId) peerDetailsTVar nodeIdMapTVar
