{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards     #-}

module Arivi.P2P.MessageHandler.NodeEndpoint (
      issueRequest
    , issueKademliaRequest
    , newIncomingConnectionHandler
) where

import           Arivi.Network                         (AriviNetworkException (..),
                                                        ConnectionHandle (..),
                                                        TransportType (..))
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.MessageHandler.Utils
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Arivi.Network.Types                   hiding(NodeId)
import           Codec.Serialise
import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async              as Async (race)
import qualified Control.Concurrent.Async.Lifted       as LA (async)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar          (putTMVar, takeTMVar)
import           Control.Exception
import qualified Control.Exception.Lifted              as LE (try)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger
import           Data.HashMap.Strict                   as HM
import           Data.Maybe                            (fromJust)
import           Control.Lens
import           Data.Proxy

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest :: forall i o m t .(HasP2PEnv m, HasLogging m, Msg t, Serialise i, Serialise o)
    => NodeId
    -> Request t i
    -> m (Response t o)
issueRequest peerNodeId req = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ readTVarIO nodeIdMapTVar
    handleOrFail <-  LE.try $ getConnectionHandle peerNodeId nodeIdMapTVar (msgType (Proxy :: Proxy (Request t i)))
    let peerDetailsTVarOrFail = HM.lookup peerNodeId nodeIdPeerMap
    case peerDetailsTVarOrFail of
        Nothing -> logWithNodeId peerNodeId "sendRequest called without adding peerNodeId: " >> throw HandlerConnectionDetailsNotFound
        Just peerDetailsTVar ->
            case handleOrFail of
                Left (e::AriviP2PException) -> $(logDebug) "getConnectionHandle failed" >> throw e
                Right connHandle -> do
                    (uuid, updatedPeerDetailsTVar) <- sendRequest peerNodeId (msgType (Proxy :: Proxy (Request t i))) connHandle peerDetailsTVar (serialise req)
                    deserialise <$> receiveResponse peerNodeId uuid updatedPeerDetailsTVar

invoke :: forall m msg r .(HasP2PEnv m, HasLogging m, Serialise msg, Serialise r)
    => Request Rpc (RpcRequest msg r)
    -> m (Response Rpc (RpcResponse msg r))
invoke req = issueRequest (undefined) req

-- | Send the p2p payload to the given NodeId
-- | NodeId should be always added to the nodeIdToPeerMap before calling this function
sendRequest :: forall m .(HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> ConnectionHandle
    -> TVar PeerDetails
    -> P2PPayload
    -> m (P2PUUID, TVar PeerDetails)
sendRequest peerNodeId messageType connHandle peerDetailsTVar payload = do
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
        Right () -> return (newuuid, peerDetailsTVar)

-- | Wait for response from the peer on the given uuid and then return the p2p message or throw an exception.
receiveResponse :: (HasLogging m)
    => NodeId
    -> P2PUUID
    -> TVar PeerDetails
    -> m P2PPayload
receiveResponse _ uuid peerDetailsTVar = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    case peerDetails ^. uuidMap.at uuid of
        Nothing -> $(logDebug) "uuid not added to peer's uuidMap" >> throw HandlerUuidNotFound
        Just mvar -> do
            winner <- liftIO $ Async.race (threadDelay 30000000) (takeMVar mvar :: IO P2PMessage)
            liftIO $ atomically $ modifyTVar' peerDetailsTVar (deleteFromUUIDMap uuid)
            case winner of
                Left _ -> $(logDebug) "response timed out" >> throw HandlerSendMessageTimeout
                Right p2pMessage -> return (payload p2pMessage)

-- | Called by kademlia. Adds a default PeerDetails record into hashmap before calling generic issueRequest
issueKademliaRequest :: (HasP2PEnv m, HasLogging m, Serialise msg)
    => NetworkConfig
    -> Request Kademlia msg
    -> m (Response Kademlia msg)
issueKademliaRequest nc payload = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    peerExists <- liftIO $ doesPeerExist nodeIdMapTVar (nc ^. nodeId)
    case peerExists of
        True -> issueRequest (nc ^. nodeId) payload
        False -> do
            peerDetailsTVar <- liftIO $ atomically $ mkPeer nc UDP NotConnected
            liftIO $ atomically $ addNewPeer (nc ^. nodeId) peerDetailsTVar nodeIdMapTVar
            issueRequest (nc ^. nodeId) payload



-- | Recv from connection handle and process incoming message
readIncomingMessage :: (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar PeerDetails
    -> MessageTypeMap m
    -> m ()
readIncomingMessage connHandle peerDetailsTVar messageTypeMap = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    msgOrFail <- LE.try $ recv connHandle
    case msgOrFail of
        Left (_::AriviNetworkException) -> logWithNodeId peerNodeId "network recv failed from readIncomingMessage" >> return ()
        Right msg -> do
            _ <- LA.async (processIncomingMessage connHandle peerDetailsTVar messageTypeMap msg)
            readIncomingMessage connHandle peerDetailsTVar messageTypeMap


-- | Processes a new message from peer
processRequest :: (HasLogging m)
    => ConnectionHandle
    -> (P2PPayload -> m P2PPayload)
    -> P2PMessage
    -> NodeId
    -> m ()
processRequest connHandle handlerFunc p2pMessage peerNodeId = do
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
            case peerDetails ^. uuidMap.at (uuid p2pMessage) of
                Just mvar -> liftIO $ putMVar mvar p2pMessage
                Nothing -> do
                    -- fromJust is justified because handler should be registered
                    let func = fromJust $ HM.lookup (messageType p2pMessage) messageTypeMap
                    processRequest connHandle func p2pMessage peerNodeId

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle :: (HasLogging m, HasP2PEnv m) => NodeId -> TVar NodeIdPeerMap -> MessageType -> m ConnectionHandle
getConnectionHandle peerNodeId nodeToPeerTVar msgType = do
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeToPeerTVar
    -- should find an entry in the hashmap
    -- raise and exception if it is not found
    case HM.lookup peerNodeId nodeIdPeerMap of
        Just peerDetailsTVar -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case getHandlerByMessageType peerDetails msgType of
                Connected connHandle -> return connHandle
                NotConnected ->
                    createConnection
                        peerDetailsTVar
                        nodeToPeerTVar
                        (getTransportType msgType)
                    -- The record has been updated.
                    -- Don't use nodeIdPeerMap anymore as its an old copy
        Nothing -> throw HandlerConnectionDetailsNotFound

-- | Obtains the connectionLock on entry and then checks if connection has been made. If yes, then simply returns the connectionHandl;e else it tries to openConnection
-- | Returns the connectionHandle or throws an exception
-- | Had to perform all operations in IO. Is there a better way?
createConnection :: (HasLogging m, HasP2PEnv m) => TVar PeerDetails -> TVar NodeIdPeerMap -> TransportType -> m ConnectionHandle
createConnection peerDetailsTVar _ transportType = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    lock <- liftIO $ atomically $ takeTMVar (peerDetails ^. connectionLock)
    (updatedPeerDetails, newConnHandle) <-
        case checkConnection peerDetails transportType of
            Connected connHandle -> liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock >> return (peerDetails, connHandle)
            NotConnected ->
                case transportType of
                    TCP -> do
                        res <- openConnectionToPeer (peerDetails ^. networkConfig) transportType
                        case res of
                            Left e -> liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock >> throw (HandlerNetworkException e)
                            Right connHandle -> return (peerDetails & streamHandle .~ (Connected connHandle), connHandle)
                    UDP -> do
                        res <- openConnectionToPeer (peerDetails ^. networkConfig) transportType
                        case res of
                            Left e -> liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock >> throw (HandlerNetworkException e)
                            Right connHandle -> return (peerDetails & streamHandle .~ Connected connHandle, connHandle)

    liftIO $ atomically $ putTMVar (updatedPeerDetails ^. connectionLock) lock
    liftIO $ atomically $ writeTVar peerDetailsTVar updatedPeerDetails
    liftIO $ atomically $ updatePeer transportType (Connected newConnHandle) peerDetailsTVar
    msgTypeMap <- getMessageTypeMapP2PEnv
    _ <- LA.async $ readIncomingMessage newConnHandle peerDetailsTVar msgTypeMap
    return newConnHandle


newIncomingConnectionHandler :: (HasP2PEnv m, HasLogging m)
    => NetworkConfig
    -> TransportType
    -> ConnectionHandle
    -> m ()
newIncomingConnectionHandler nc transportType connHandle = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    msgTypeMap <- getMessageTypeMapP2PEnv
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeIdMapTVar
    -- lock <- liftIO $ atomically $ newTMVar True
    case HM.lookup (nc ^. nodeId) nodeIdPeerMap of
        Nothing -> do
            peerDetailsTVar <- liftIO $ atomically $ mkPeer nc transportType (Connected connHandle)
            liftIO $ atomically $ addNewPeer (nc ^. nodeId) peerDetailsTVar nodeIdMapTVar
        Just peerDetailsTVar -> liftIO $ atomically $ updatePeer transportType (Connected connHandle) peerDetailsTVar
    -- fromJust might be justified since we just added the entry in addPeer function above before fetching it
    nodeIdPeerMap' <- liftIO $ atomically $ readTVar nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup (nc ^. nodeId) nodeIdPeerMap')
    _ <- LA.async (readIncomingMessage connHandle peerDetailsTVar msgTypeMap)
    return ()
