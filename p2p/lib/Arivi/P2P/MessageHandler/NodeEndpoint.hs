{-# LANGUAGE NamedFieldPuns, RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs, RankNTypes, DataKinds #-}

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
import           Control.Monad.Except
import           Data.String.Conv

handler :: forall o m t. (HasP2PEnv m, HasLogging m) 
    => AriviP2PException
    -> ExceptT AriviP2PException m (Response t o)
handler e =
    case e of
        PeerNotFound -> $(logDebug) "peer not added to map" >> throwError PeerNotFound
        x -> $(logDebug) ((toS . displayException) x) >> throwError x

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest ::
       forall i o m t.
       (HasP2PEnv m, HasLogging m, Msg t, Serialise i, Serialise o)
    => NodeId
    -> Request t i
    -> ExceptT AriviP2PException m (Response t o)
issueRequest peerNodeId req = flip catchError handler $ do 
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- (lift . liftIO) $ readTVarIO nodeIdMapTVar
    connHandle <- ExceptT $ getConnectionHandle peerNodeId nodeIdMapTVar (getTransportType $ msgType (Proxy :: Proxy (Request t i)))
    peerDetailsTVar <- maybe (throwError PeerNotFound) return (nodeIdPeerMap ^.at peerNodeId) 
    case req of
        RpcRequest msg -> do 
            uuid <- ExceptT $ sendRequest peerNodeId (msgType (Proxy :: Proxy (Request t i))) connHandle peerDetailsTVar (serialise msg)
            resp <- ExceptT $ receiveResponse peerDetailsTVar uuid
            rpcResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (RpcResponse rpcResp)

        OptionRequest msg -> do 
            uuid <- ExceptT $ sendRequest peerNodeId (msgType (Proxy :: Proxy (Request t i))) connHandle peerDetailsTVar (serialise msg)
            resp <- ExceptT $ receiveResponse peerDetailsTVar uuid
            rpcResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (OptionResponse rpcResp)

        KademliaRequest msg -> do 
            uuid <- ExceptT $ sendRequest peerNodeId (msgType (Proxy :: Proxy (Request t i))) connHandle peerDetailsTVar (serialise msg)
            resp <- ExceptT $ receiveResponse peerDetailsTVar uuid
            rpcResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (KademliaResponse rpcResp)
        
            
-- | Send the p2p payload to the given NodeId
-- | NodeId should be always added to the nodeIdToPeerMap before calling this function
sendRequest :: forall m .(HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> ConnectionHandle
    -> TVar PeerDetails
    -> P2PPayload
    -> m (Either AriviP2PException P2PUUID)
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
            return (Left $ NetworkException e)
        Right () -> return (Right newuuid)

-- | Wait for response from the peer on the given uuid and then return the p2p message or throw an exception.
receiveResponse :: (HasLogging m)
    => TVar PeerDetails
    -> P2PUUID
    -> m (Either AriviP2PException P2PPayload)
receiveResponse peerDetailsTVar uuid = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    case peerDetails ^. uuidMap.at uuid of
        Nothing -> $(logDebug) "uuid not added to peer's uuidMap" >> return  (Left InvalidUuid)
        Just mvar -> do
            winner <- liftIO $ Async.race (threadDelay 30000000) (takeMVar mvar :: IO P2PMessage)
            liftIO $ atomically $ modifyTVar' peerDetailsTVar (deleteFromUUIDMap uuid)
            case winner of
                Left _ -> $(logDebug) "response timed out" >> return  (Left SendMessageTimeout)
                Right p2pMessage -> return $ Right (payload p2pMessage)

-- | Called by kademlia. Adds a default PeerDetails record into hashmap before calling generic issueRequest
issueKademliaRequest :: (HasP2PEnv m, HasLogging m, Serialise msg)
    => NetworkConfig
    -> Request 'Kademlia msg
    -> ExceptT AriviP2PException m (Response 'Kademlia msg)
issueKademliaRequest nc payload = do
    nodeIdMapTVar <- lift $ getNodeIdPeerMapTVarP2PEnv
    peerExists <- (lift . liftIO) $ doesPeerExist nodeIdMapTVar (nc ^. nodeId)
    case peerExists of
        True -> issueRequest (nc ^. nodeId) payload
        False -> do
            peerDetailsTVar <- (lift . liftIO) $ atomically $ mkPeer nc UDP NotConnected
            (lift . liftIO) $ atomically $ addNewPeer (nc ^. nodeId) peerDetailsTVar nodeIdMapTVar
            issueRequest (nc ^. nodeId) payload

-- | Recv from connection handle and process incoming message
readIncomingMessage :: (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar PeerDetails
    -> Handlers
    -> m ()
readIncomingMessage connHandle peerDetailsTVar messageTypeMap = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    msgOrFail <- LE.try $ recv connHandle
    case msgOrFail of
        Left (_::AriviNetworkException) -> logWithNodeId peerNodeId "network recv failed from readIncomingMessage" >> return ()
        Right msg -> do
            _ <- LA.async (processIncomingMessage connHandle peerDetailsTVar messageTypeMap msg)
            readIncomingMessage connHandle peerDetailsTVar messageTypeMap


-- | Processes a new request from peer
processRequest :: (HasLogging m)
    => ConnectionHandle
    -> (P2PPayload -> m P2PPayload)
    -> P2PMessage
    -> NodeId
    -> m (Either AriviP2PException ())
processRequest connHandle handlerFunc p2pMessage peerNodeId = do
    responseMsg <- handlerFunc (payload p2pMessage) -- handler should handle all its own exceptions. No exception should reach here.
    let p2pResponse = generateP2PMessage (uuid p2pMessage) (messageType p2pMessage) responseMsg
    res <- LE.try $ send connHandle (serialise p2pResponse)
    case res of
        Left (e::AriviNetworkException) -> logWithNodeId peerNodeId "network send failed while sending response" >> return (Left (NetworkException e))
        Right _                         -> return (Right ())


-- | Takes an incoming message from the network layer and procesess it in 2 ways. If the mes0sage was an expected reply, it is put into the waiting mvar or else the appropriate handler for the message type is called and the generated response is sent back
processIncomingMessage :: (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar PeerDetails
    -> Handlers
    -> P2PPayload
    -> m (Either AriviP2PException ())
processIncomingMessage connHandle peerDetailsTVar messageTypeMap msg = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    let p2pMessageOrFail = deserialiseOrFail msg
    case p2pMessageOrFail of
        Left _ -> logWithNodeId peerNodeId "Peer sent malformed msg" >> return  (Left DeserialiseFailureP2P)
        Right p2pMessage -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case peerDetails ^. uuidMap.at (uuid p2pMessage) of
                Just mvar -> liftIO $ putMVar mvar p2pMessage >> return (Right ())
                Nothing -> do
                    --let func = fromJust $ HM.lookup (messageType p2pMessage) messageTypeMap
                    let func = case messageType p2pMessage of
                          Rpc -> rpc messageTypeMap
                          Kademlia -> kademlia messageTypeMap
                    processRequest connHandle func p2pMessage peerNodeId

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle :: (HasLogging m, HasP2PEnv m) => NodeId -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
getConnectionHandle peerNodeId nodeToPeerTVar transportType = do
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeToPeerTVar
    -- should find an entry in the hashmap
    -- exception if it is not found
    case HM.lookup peerNodeId nodeIdPeerMap of
        Just peerDetailsTVar -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case getHandlerByMessageType peerDetails transportType of
                Connected connHandle -> return (Right connHandle)
                NotConnected -> createConnection peerDetailsTVar nodeToPeerTVar transportType
        Nothing -> return (Left PeerNotFound) 

-- | Obtains the connectionLock on entry and then checks if connection has been made. If yes, then simply returns the connectionHandl;e else it tries to openConnection
-- | Returns the connectionHandle or an exception
createConnection :: (HasLogging m, HasP2PEnv m) => TVar PeerDetails -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
createConnection peerDetailsTVar _ transportType = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    lock <- liftIO $ atomically $ takeTMVar (peerDetails ^. connectionLock)
    connHandleEither <-
        case checkConnection peerDetails transportType of
            Connected connHandle -> return $ Right connHandle
            NotConnected -> networkToP2PException <$> openConnectionToPeer (peerDetails ^. networkConfig) transportType 
                        
    msgTypeMap <- getMessageTypeMapP2PEnv
    liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock
    case connHandleEither of
        Right c -> do
            liftIO $ atomically $ updatePeer transportType (Connected c) peerDetailsTVar
            _ <- LA.async $ readIncomingMessage c peerDetailsTVar msgTypeMap
            return (Right c)
        Left e -> return (Left e)


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
    nodeIdPeerMap' <- liftIO $ atomically $ readTVar nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup (nc ^. nodeId) nodeIdPeerMap')
    readIncomingMessage connHandle peerDetailsTVar msgTypeMap
