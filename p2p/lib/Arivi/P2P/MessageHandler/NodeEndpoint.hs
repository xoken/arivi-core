{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Arivi.P2P.MessageHandler.NodeEndpoint (
      sendRequestAndGetResponse
    , newIncomingConnectionHandler
) where

import           Arivi.Network                         (AriviNetworkException (..),
                                                        ConnectionHandle (..),
                                                        TransportType (..),
                                                        openConnection)
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.Utils.Logging
import           Codec.Serialise
import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async              as Async (race)
import qualified Control.Concurrent.Async.Lifted       as LA (async)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar          (putTMVar, takeTMVar)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as LE (try)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger
import           Data.HashMap.Strict                   as HM
import           Data.Maybe                            (fromJust)
import           Data.String.Conv
import           Data.Text                             as T
import qualified Data.UUID                             as UUID (toString)
import           Data.UUID.V4                          (nextRandom)
import           Network.Socket                        (PortNumber)

sendRequestAndGetResponse :: (HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> P2PPayload
    -- -> Maybe Int
    -> m P2PPayload
sendRequestAndGetResponse peerNodeId messageType payload = do
    -- Can check if nodeId is present in map before calling sendRequest
    uuid <- sendRequest peerNodeId messageType payload
    receiveResponse peerNodeId uuid

-- | Send the p2p payload to the given NodeId
-- | NodeId should be always added to the nodeIdToPeerMap before calling this function
sendRequest :: forall m .(HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> P2PPayload
    -> m P2PUUID
sendRequest peerNodeId messageType payload = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeIdMapTVar
    handleOrFail <-  LE.try $ getConnectionHandle peerNodeId nodeIdMapTVar messageType
    let peerDetailsTVarOrFail = HM.lookup peerNodeId nodeIdPeerMap
    case peerDetailsTVarOrFail of
        Nothing -> logWithNodeId peerNodeId "sendRequest called without adding peerNodeId: " >> throw HandlerConnectionDetailsNotFound
        Just peerDetailsTVar ->
            case handleOrFail of
                Left (e::AriviP2PException) -> $(logDebug) "getConnectionHandle failed" >> throw e
                Right connHandle -> do
                    newuuid <- liftIO getUUID
                    mvar <- liftIO newEmptyMVar
                    liftIO $ atomically $ modifyTVar' peerDetailsTVar (insertToUUIDMap newuuid mvar)
                    let p2pMessage = generateP2PMessage newuuid messageType payload
                    res <- LE.try (send connHandle (serialise p2pMessage))
                    case res of
                        Left (e::AriviNetworkException) -> do
                            liftIO $ atomically $ modifyTVar' peerDetailsTVar (deleteFromUUIDMap newuuid)
                            logWithNodeId peerNodeId "network send failed from sendRequst for "
                            throw e
                        Right () -> return newuuid

logWithNodeId :: (HasLogging m) => NodeId -> String -> m ()
logWithNodeId peerNodeId logLine = $(logDebug) $ toS $ logLine ++ show peerNodeId
-- | Wait for response from the peer on the given uuid and then return the p2p message or throw an exception. TODO: Refactor to remove repetition
receiveResponse :: (HasLogging m,HasP2PEnv m)
    => NodeId
    -> P2PUUID
    -> m P2PPayload
receiveResponse peerNodeId uuid = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeIdMapTVar
    let peerDetailsTVarOrFail = HM.lookup peerNodeId nodeIdPeerMap
    case peerDetailsTVarOrFail of
        Nothing -> $(logDebug) "receiveResponse called without adding peerNodeId to map" >> throw HandlerConnectionDetailsNotFound
        Just peerDetailsTVar -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case HM.lookup uuid (uuidMap peerDetails) of
                Nothing -> $(logDebug) "uuid not added to peer's uuidMap" >> throw HandlerUuidNotFound
                Just mvar -> do
                    winner <- liftIO $ Async.race (threadDelay 30000000) (takeMVar mvar :: IO P2PMessage)
                    liftIO $ atomically $ modifyTVar' peerDetailsTVar (deleteFromUUIDMap uuid)
                    case winner of
                        Left _ -> $(logDebug) "response timed out" >> throw HandlerSendMessageTimeout
                        Right p2pMessage -> return (payload p2pMessage)


getNodeId :: TVar PeerDetails -> IO NodeId
getNodeId peerDetailsTVar =
    nodeId <$> readTVarIO peerDetailsTVar

readIncomingMessage :: (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar PeerDetails
    -> MessageTypeMap m
    -> m ()
readIncomingMessage connHandle peerDetailsTVar messageTypeMap = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    msgOrFail <- LE.try $ recv connHandle
    case msgOrFail of
        Left (e::AriviNetworkException) -> logWithNodeId peerNodeId "network recv failed from readIncomingMessage" >> return ()
        Right msg -> do
            _ <- LA.async (processIncomingMessage connHandle peerDetailsTVar messageTypeMap msg)
            readIncomingMessage connHandle peerDetailsTVar messageTypeMap



processResponse :: (HasLogging m, HasP2PEnv m)
    => ConnectionHandle
    -> (P2PPayload -> m P2PPayload)
    -> P2PMessage
    -> NodeId
    -> m ()
processResponse connHandle handlerFunc p2pMessage peerNodeId = do
    responseMsg <- handlerFunc (payload p2pMessage) -- handler should handle all its own exceptions. No exception should reach here.
    let p2pResponse = generateP2PMessage (uuid p2pMessage) (messageType p2pMessage) responseMsg
    res <- LE.try $ send connHandle (serialise p2pResponse)
    case res of
        Left (e::AriviNetworkException) -> logWithNodeId peerNodeId "network send failed while sending response" >> throw e
        Right _                         -> return ()


-- | Takes an incoming message from the network layer and procesess it in 2 ways. If the message was an expected reply, it is put into the waiting mvar or else the appropriate handler for the message type is called and the generated response is sent back
processIncomingMessage :: (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar PeerDetails
    -> MessageTypeMap m
    -> P2PPayload
    -> m ()
processIncomingMessage connHandle peerDetailsTVar messageTypeMap msg = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    let p2pMessageOrFail = deserialiseOrFail msg
    case p2pMessageOrFail of
        Left _ -> logWithNodeId peerNodeId "Peer sent malformed msg" >> throw P2PDeserialisationException
        Right p2pMessage -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case HM.lookup (uuid p2pMessage) (uuidMap peerDetails) of
                Just mvar -> liftIO $ putMVar mvar p2pMessage
                Nothing -> do
                    -- fromJust is justified because handler should be registered
                    let func = fromJust $ HM.lookup (messageType p2pMessage) messageTypeMap
                    processResponse connHandle func p2pMessage peerNodeId

-- | wraps the payload with message type { Kademlia | RPC | PubSub} and UUID
generateP2PMessage :: P2PUUID -> MessageType -> P2PPayload -> P2PMessage
generateP2PMessage = P2PMessage

insertToUUIDMap :: P2PUUID -> MVar P2PMessage -> PeerDetails -> PeerDetails
insertToUUIDMap uuid mvar peerDetails = peerDetails {uuidMap = HM.insert uuid mvar (uuidMap peerDetails)}

deleteFromUUIDMap :: P2PUUID -> PeerDetails -> PeerDetails
deleteFromUUIDMap uuid peerDetails = peerDetails {uuidMap = HM.delete uuid (uuidMap peerDetails)}

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle :: (HasLogging m, HasP2PEnv m) => NodeId -> TVar NodeIdPeerMap -> MessageType -> m ConnectionHandle
getConnectionHandle peerNodeId nodeToPeerTVar msgType = do
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeToPeerTVar
    -- should find an entry in the hashmap
    -- raise and exception if it is not found
    case HM.lookup peerNodeId nodeIdPeerMap of
        Just peerDetailsTVar -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            let handle = getHandlerByMessageType peerDetails msgType
            case handle of
                Connected connHandle -> return connHandle
                NotConnected ->
                    createConnection peerDetailsTVar (getTransportType msgType)
                    -- The record has been updated.
                    -- Don't use nodeIdPeerMap anymore as its an old copy
        Nothing -> throw HandlerConnectionDetailsNotFound

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

-- | Obtains the connectionLock on entry and then checks if connection has been made. If yes, then simply returns the connectionHandl;e else it tries to openConnection
-- | Returns the connectionHandle or throws an exception
-- | Had to perform all operations in IO. Is there a better way?
createConnection :: (HasLogging m, HasP2PEnv m) => TVar PeerDetails -> TransportType -> m ConnectionHandle
createConnection peerDetailsTVar transportType = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    lock <- liftIO $ atomically $ takeTMVar (connectionLock peerDetails)
    (updatedPeerDetails, newConnHandle) <-
        case checkConnection peerDetails transportType of
            Connected connHandle -> liftIO $ atomically $ putTMVar (connectionLock peerDetails) lock >> return (peerDetails, connHandle)
            NotConnected ->
                case transportType of
                    TCP -> do
                        res <- openConnectionToPeer (ip' peerDetails) (fromJust $ tcpPort peerDetails) transportType (nodeId peerDetails)
                        case res of
                            Left e -> liftIO $ atomically $ putTMVar (connectionLock peerDetails) lock >> throw (HandlerNetworkException e)
                            Right connHandle -> return (peerDetails {streamHandle = Connected connHandle}, connHandle)
                    UDP -> do
                        res <- openConnectionToPeer (ip' peerDetails) (fromJust $ udpPort peerDetails) transportType (nodeId peerDetails)
                        case res of
                            Left e -> liftIO $ atomically $ putTMVar (connectionLock peerDetails) lock >> throw (HandlerNetworkException e)
                            Right connHandle -> return (peerDetails {streamHandle = Connected connHandle}, connHandle)

    liftIO $ atomically $ putTMVar (connectionLock updatedPeerDetails) lock
    liftIO $ atomically $ writeTVar peerDetailsTVar updatedPeerDetails
    msgTypeMap <- getMessageTypeMapP2PEnv
    _ <- LA.async $ readIncomingMessage newConnHandle peerDetailsTVar msgTypeMap
    return newConnHandle

-- | Get a random unique id
getUUID :: IO P2PUUID
getUUID = UUID.toString <$> nextRandom

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
addPeer peerNodeId peerIp portNum transportType connHandle tvar = do
    nodeIdPeerMap <- readTVar tvar
    lock <- newTMVar True
    case HM.lookup peerNodeId nodeIdPeerMap of
        Nothing -> do
            let peerDetails = PeerDetails {
                    nodeId = peerNodeId
                , rep' = 0.0 -- needs to be a float. Not a Maybe Int
                , ip' = peerIp
                , udpPort = case transportType of
                                UDP -> Just portNum
                                _   -> Nothing
                , tcpPort = case transportType of
                                TCP -> Just portNum
                                _   -> Nothing
                , streamHandle = case transportType of
                                    TCP -> Connected connHandle
                                    UDP -> NotConnected
                , datagramHandle = case transportType of
                                        UDP -> Connected connHandle
                                        TCP -> NotConnected
                , uuidMap = HM.empty
                , connectionLock = lock
            }
            peerDetailsTVar <- newTVar peerDetails
            let updatedHM = HM.insert peerNodeId peerDetailsTVar nodeIdPeerMap
            writeTVar tvar updatedHM
        Just peerDetailsTVar -> do
            modifyTVar' peerDetailsTVar updateConnHandle
            updatedPeerMap <- readTVar tvar
            let updatedTVar = fromJust $ HM.lookup peerNodeId updatedPeerMap
            let updatedHM = HM.insert peerNodeId updatedTVar nodeIdPeerMap
            writeTVar tvar updatedHM
            where
                -- Assert if IP is the same
                updateConnHandle peerDetails =
                    if transportType == TCP then
                        peerDetails {streamHandle = Connected connHandle}
                    else peerDetails {datagramHandle = Connected connHandle}


newIncomingConnectionHandler :: (HasP2PEnv m, HasLogging m)
    => NodeId
    -> IP
    -> PortNumber
    -> TransportType
    -> ConnectionHandle
    -> m ()
newIncomingConnectionHandler peerNodeId peerIP portNum transportType connHandle = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    msgTypeMap <- getMessageTypeMapP2PEnv
    liftIO $ atomically $ addPeer peerNodeId peerIP portNum transportType connHandle nodeIdMapTVar
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    -- fromJust might be justified since we just added the entry in addPeer function above before fetching it
    let peerDetailsTVar = fromJust (HM.lookup peerNodeId nodeIdMap)
    _ <- LA.async (readIncomingMessage connHandle peerDetailsTVar msgTypeMap)
    return ()
