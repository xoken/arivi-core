{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Arivi.P2P.MessageHandler.NodeEndpoint
    ( issueRequest
    , issueSend
    , issueKademliaRequest
    ) where

import Arivi.Network (AriviNetworkException, ConnectionHandle(..), TransportType(..))
import Arivi.Network.Types hiding (NodeId)
import Arivi.P2P.Connection
import Arivi.P2P.Exception
import Arivi.P2P.MessageHandler.HandlerTypes hiding (messageType, payload, uuid)
import Arivi.P2P.MessageHandler.Utils
import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Arivi.Utils.Logging

import Codec.Serialise
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async (race)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception.Lifted as LE (try)
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Data.Proxy

sendWithoutUUID ::
       (HasNodeEndpoint m, HasLogging m)
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
        Left (e :: AriviNetworkException) -> do
            logWithNodeId peerNodeId "network send failed from sendWithoutUUID for "
            return (Left $ NetworkException e)
        Right a -> return (Right a)

sendAndReceive ::
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
                Right (P2PMessage _ _ payl) -> return (Right payl)

-- | Send a message without waiting for any response or registering a uuid.
-- | Useful for pubsub notifies and publish. To be called by the rpc/pubsub and kademlia handlers on getting a new request
issueSend ::
       forall env m r topic rmsg pmsg t i. (Serialise pmsg, Show topic)
    => (HasP2PEnv env m r topic rmsg pmsg, Msg t, Serialise (Request t i)) =>
           NodeId -> Maybe P2PUUID -> Request t i -> ExceptT AriviP2PException m ()
issueSend peerNodeId uuid req = do
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    connHandle <-
        ExceptT $
        getConnectionHandle peerNodeId nodeIdMapTVar (getTransportType $ msgType (Proxy :: Proxy (Request t i)))
    ExceptT $ sendWithoutUUID peerNodeId (msgType (Proxy :: Proxy (Request t i))) uuid connHandle (serialise req)

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest ::
       forall env m r topic rmsg pmsg i o t. (Serialise pmsg, Show topic)
    => (HasP2PEnv env m r topic rmsg pmsg, Msg t, Serialise (Request t i), Serialise (Response t o)) =>
           NodeId -> Request t i -> ExceptT AriviP2PException m (Response t o)
issueRequest peerNodeId req = do
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ readTVarIO nodeIdMapTVar
    connHandle <-
        ExceptT $
        getConnectionHandle peerNodeId nodeIdMapTVar (getTransportType $ msgType (Proxy :: Proxy (Request t i)))
    peerDetailsTVar <- maybe (throwError PeerNotFound) return (nodeIdPeerMap ^. at peerNodeId)
    resp <- ExceptT $ sendAndReceive peerDetailsTVar (msgType (Proxy :: Proxy (Request t i))) connHandle (serialise req)
    ExceptT $ (return . safeDeserialise . deserialiseOrFail) resp

-- | Called by kademlia. Adds a default PeerDetails record into hashmap before calling generic issueRequest
issueKademliaRequest ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg, Serialise msg) =>
           NetworkConfig -> Request 'Kademlia msg -> ExceptT AriviP2PException m (Response 'Kademlia msg)
issueKademliaRequest nc payload = do
    nodeIdMapTVar <- lift getNodeIdPeerMapTVarP2PEnv
    peerExists <- (lift . liftIO) $ doesPeerExist nodeIdMapTVar (nc ^. nodeId)
    if peerExists
        then issueRequest (nc ^. nodeId) payload
        else (do (lift . liftIO) $ atomically $ addPeerToMap nc UDP nodeIdMapTVar
                 issueRequest (nc ^. nodeId) payload)
