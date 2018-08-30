{-# LANGUAGE NamedFieldPuns, RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs, RankNTypes, DataKinds #-}

module Arivi.P2P.MessageHandler.NodeEndpoint (
      issueRequest
    , issueSend
    , issueKademliaRequest
    , newIncomingConnectionHandler
) where

import           Arivi.Network                         (AriviNetworkException (..),
                                                        ConnectionHandle (..),
                                                        TransportType (..), HasSecretKey(..))
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

handler :: forall o m t. (HasNodeEndpoint m, HasLogging m) 
    => AriviP2PException
    -> ExceptT AriviP2PException m (Response t o)
handler e =
    case e of
        PeerNotFound -> $(logDebug) "peer not added to map" >> throwError PeerNotFound
        x -> $(logDebug) ((toS . displayException) x) >> throwError x

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest ::
       forall i o m t r msg.
       (HasNodeEndpoint m, HasLogging m, Msg t, Serialise i, Serialise o, HasResourceAndService m r msg)
    => NodeId
    -> Request t i
    -> ExceptT AriviP2PException m (Response t o)
issueRequest peerNodeId req = do 
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ readTVarIO nodeIdMapTVar
    connHandle <- ExceptT $ getConnectionHandle peerNodeId nodeIdMapTVar (getTransportType $ msgType (Proxy :: Proxy (Request t i)))
    peerDetailsTVar <- maybe (throwError PeerNotFound) return (nodeIdPeerMap ^.at peerNodeId)
    case req of
        RpcRequest msg -> do
            resp <- ExceptT $ sendAndReceive peerDetailsTVar (msgType (Proxy :: Proxy (Request t i))) connHandle (serialise msg)
            rpcResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (RpcResponse rpcResp)

        OptionRequest msg -> do 
            resp <- ExceptT $ sendAndReceive peerDetailsTVar (msgType (Proxy :: Proxy (Request t i))) connHandle (serialise msg)
            optionResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (OptionResponse optionResp) 

        KademliaRequest msg -> do 
            resp <- ExceptT $ sendAndReceive peerDetailsTVar (msgType (Proxy :: Proxy (Request t i))) connHandle (serialise msg)
            kademliaResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (KademliaResponse kademliaResp) 

        PubSubRequest msg -> do 
            resp <- ExceptT $ sendAndReceive peerDetailsTVar (msgType (Proxy :: Proxy (Request t i))) connHandle (serialise msg)
            pubsubResp <- ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp
            return (PubSubResponse pubsubResp)

-- | Called by kademlia. Adds a default PeerDetails record into hashmap before calling generic issueRequest
issueKademliaRequest :: (HasNodeEndpoint m, HasLogging m, Serialise msg)
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

-- | Send a message without waiting for any response or registering a uuid.
-- | Useful for pubsub notifies and publish. To be called by the rpc/pubsub and kademlia handlers on getting a new request
issueSend :: forall i m t.
       (HasNodeEndpoint m, HasLogging m, Msg t, Serialise i)
    => NodeId
    -> Maybe P2PUUID
    -> Request t i
    -> ExceptT AriviP2PException m ()
issueSend peerNodeId uuid req = do
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    connHandle <- ExceptT $ getConnectionHandle peerNodeId nodeIdMapTVar (getTransportType $ msgType (Proxy :: Proxy (Request t i)))
    case req of
        RpcRequest msg -> ExceptT $ sendWithoutUUID peerNodeId (msgType (Proxy :: Proxy (Request t i))) uuid connHandle (serialise msg)
        OptionRequest msg -> ExceptT $ sendWithoutUUID peerNodeId (msgType (Proxy :: Proxy (Request t i))) uuid connHandle (serialise msg)
        KademliaRequest msg -> ExceptT $ sendWithoutUUID peerNodeId (msgType (Proxy :: Proxy (Request t i))) uuid connHandle (serialise msg)
        PubSubRequest msg -> ExceptT $ sendWithoutUUID peerNodeId (msgType (Proxy :: Proxy (Request t i))) uuid connHandle (serialise msg)

sendWithoutUUID ::forall m .(HasNodeEndpoint m, HasLogging m)
    => NodeId
    -> MessageType
    -> Maybe P2PUUID
    -> ConnectionHandle
    -> P2PPayload
    -> m (Either AriviP2PException ())
sendWithoutUUID peerNodeId messageType uuid connHandle payload = do
    let p2pMessage = generateP2PMessage uuid messageType payload
    res <- LE.try (send connHandle (serialise p2pMessage))
    case res of
        Left (e::AriviNetworkException) -> do
            logWithNodeId peerNodeId "network send failed from sendWithoutUUID for "
            return (Left $ NetworkException e)
        Right a -> return (Right a)

sendAndReceive :: forall m.
       (HasNodeEndpoint m, HasLogging m)
    => TVar PeerDetails
    -> MessageType
    -> ConnectionHandle
    -> P2PPayload
    -> m (Either AriviP2PException P2PPayload)
sendAndReceive peerDetailsTVar messageType connHandle msg = do
    uuid <- liftIO getUUID
    mvar <- liftIO newEmptyMVar
    liftIO $ atomically $ modifyTVar' peerDetailsTVar (insertToUUIDMap uuid mvar)
    let p2pMessage = generateP2PMessage (Just uuid) messageType msg
    res <- networkToP2PException <$> LE.try (send connHandle (serialise p2pMessage))
    case res of
        Left e -> do
            liftIO $ atomically $ modifyTVar' peerDetailsTVar (deleteFromUUIDMap uuid)
            return (Left e)
        Right () -> do
            winner <- liftIO $ Async.race (threadDelay 30000000) (takeMVar mvar :: IO P2PMessage)
            case winner of
                Left _ -> $(logDebug) "response timed out" >> return (Left SendMessageTimeout)
                Right p2pMessage -> return (Right $ payload p2pMessage)



-- | Recv from connection handle and process incoming message
readIncomingMessage :: forall m r msg . (HasNodeEndpoint m, HasLogging m, HasResourceAndService m r msg)
    => ConnectionHandle
    -> TVar PeerDetails
    -> m ()
readIncomingMessage connHandle peerDetailsTVar = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    msgOrFail <- LE.try $ recv connHandle
    case msgOrFail of
        Left (_::AriviNetworkException) -> logWithNodeId peerNodeId "network recv failed from readIncomingMessage" >> return ()
        Right msg -> do
            _ <- LA.async (processIncomingMessage connHandle peerDetailsTVar msg)
            readIncomingMessage connHandle peerDetailsTVar


globalHandler :: forall m r msg t . (HasResourceAndService m r msg) 
    => Request t ByteString
    -> m (Response t ByteString)
globalHandler req = do
    resource <- getResourceType
    message <- getServiceMessageType
    handlers <- getHandlers
    case req of 
        RpcRequest payload -> (rpc handlers) resource message payload
        KademliaRequest payload -> (kademlia handlers) payload


-- | Processes a new request from peer
processRequest :: forall m r msg . (HasLogging m, HasNodeEndpoint m, HasResourceAndService m r msg)
    => ConnectionHandle
    -> P2PMessage
    -> NodeId
    -> m (Either AriviP2PException ())
processRequest connHandle p2pMessage peerNodeId = do
    responseMsg <- handlerFunc (payload p2pMessage) -- handler should handle all its own exceptions. No exception should reach here.
    let p2pResponse = generateP2PMessage (uuid p2pMessage) (messageType p2pMessage) responseMsg
    res <- LE.try $ send connHandle (serialise p2pResponse)
    case res of
        Left (e::AriviNetworkException) -> logWithNodeId peerNodeId "network send failed while sending response" >> return (Left (NetworkException e))
        Right _                         -> return (Right ())


-- | Takes an incoming message from the network layer and procesess it in 2 ways. If the mes0sage was an expected reply, it is put into the waiting mvar or else the appropriate handler for the message type is called and the generated response is sent back
processIncomingMessage :: forall m r msg . (HasNodeEndpoint m, HasLogging m, HasResourceAndService m r msg)
    => ConnectionHandle
    -> TVar PeerDetails
    -> P2PPayload
    -> m (Either AriviP2PException ())
processIncomingMessage connHandle peerDetailsTVar msg = do
    peerNodeId <- liftIO $ getNodeId peerDetailsTVar
    let p2pMessageOrFail = deserialiseOrFail msg
    case p2pMessageOrFail of
        Left _ -> logWithNodeId peerNodeId "Peer sent malformed msg" >> return  (Left DeserialiseFailureP2P)
        Right p2pMessage -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case (uuid p2pMessage) of
                Just uuid ->
                    case peerDetails ^. uuidMap.at uuid of
                        Just mvar -> liftIO $ putMVar mvar p2pMessage >> return (Right ())
                        Nothing -> return (Left InvalidUuid)
                Nothing ->
                    --let func = fromJust $ HM.lookup (messageType p2pMessage) messageTypeMap
                    -- let func = case messageType p2pMessage of
                    --       Rpc -> rpc messageTypeMap
                    --       Kademlia -> kademlia messageTypeMap
                    processRequest connHandle p2pMessage peerNodeId

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle :: forall m r msg . (HasLogging m, HasNodeEndpoint m, HasResourceAndService m r msg) => NodeId -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
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
createConnection :: forall m r msg . (HasLogging m, HasNodeEndpoint m, HasResourceAndService m r msg) => TVar PeerDetails -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
createConnection peerDetailsTVar _ transportType = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    lock <- liftIO $ atomically $ takeTMVar (peerDetails ^. connectionLock)
    connHandleEither <-
        case checkConnection peerDetails transportType of
            Connected connHandle -> return $ Right connHandle
            NotConnected -> networkToP2PException <$> openConnectionToPeer (peerDetails ^. networkConfig) transportType 
                        
    -- msgTypeMap <- getMessageTypeMapP2PEnv
    liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock
    case connHandleEither of
        Right c -> do
            liftIO $ atomically $ updatePeer transportType (Connected c) peerDetailsTVar
            _ <- LA.async $ readIncomingMessage c peerDetailsTVar
            return (Right c)
        Left e -> return (Left e)


newIncomingConnectionHandler :: forall m r msg . (HasNodeEndpoint m, HasLogging m, HasResourceAndService m r msg)
    => NetworkConfig
    -> TransportType
    -> ConnectionHandle
    -> m ()
newIncomingConnectionHandler nc transportType connHandle = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    -- msgTypeMap <- getMessageTypeMapP2PEnv
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeIdMapTVar
    -- lock <- liftIO $ atomically $ newTMVar True
    case HM.lookup (nc ^. nodeId) nodeIdPeerMap of
        Nothing -> do
            peerDetailsTVar <- liftIO $ atomically $ mkPeer nc transportType (Connected connHandle)
            liftIO $ atomically $ addNewPeer (nc ^. nodeId) peerDetailsTVar nodeIdMapTVar
        Just peerDetailsTVar -> liftIO $ atomically $ updatePeer transportType (Connected connHandle) peerDetailsTVar
    nodeIdPeerMap' <- liftIO $ atomically $ readTVar nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup (nc ^. nodeId) nodeIdPeerMap')
    readIncomingMessage connHandle peerDetailsTVar
