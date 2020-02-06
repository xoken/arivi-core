<<<<<<< HEAD
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
=======
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.PubSub.Handler
    ( pubSubHandler
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
<<<<<<< HEAD
import Arivi.P2P.Types
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Types

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens
import Control.Monad.Reader
import Data.Hashable
import Data.ByteString.Lazy
import qualified Data.Set as Set

-- |Handler functions might never halt, they should be run
-- in a separate thread.

pubSubHandler ::
       ( MonadReader env m
       , HasPubSub env t msg
       , MonadIO m
       )
    => NodeId
    -> PubSub
    -> ByteString
    -> m ByteString
=======
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Notify
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent.MVar ()
import Control.Concurrent.STM.TVar ()
import Control.Lens ()
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader
import Data.ByteString.Lazy

-- |Handler functions might never halt, they should be run
-- in a separate thread.
pubSubHandler ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> PubSub -> ByteString -> m ByteString
>>>>>>> breaking out arivi-core from arivi
pubSubHandler nid Subscribe req = serialise <$> subscribeHandler nid (deserialise req)
pubSubHandler nid Publish req = serialise <$> publishHandler nid (deserialise req)
pubSubHandler nid Notify req = serialise <$> notifyHandler nid (deserialise req)

notifyHandler ::
<<<<<<< HEAD
       ( MonadReader env m
       , HasPubSub env t msg, MonadIO m)
    => NodeId
    -> Request ('PubSub 'Notify) (PubSubPayload t msg)
    -> m (Response ('PubSub 'Notify) (PubSubPayload t msg))
notifyHandler nid (PubSubRequest (PubSubPayload (t, msg))) = do
    inboxed <- asks inbox
    cached <- asks cache
    handlers <- asks topicHandlers
    PubSubResponse . PubSubPayload . (t, ) <$> handleTopic nid inboxed cached handlers t msg

publishHandler ::
       ( MonadReader env m
       , HasPubSub env t msg, MonadIO m)
    => NodeId
    -> Request ('PubSub 'Publish) (PubSubPayload t msg)
    -> m (Response ('PubSub 'Publish) (PubSubPayload t msg))
publishHandler nid (PubSubRequest (PubSubPayload (t, msg))) = do
    inboxed <- asks inbox
    cached <- asks cache
    handlers <- asks topicHandlers
    PubSubResponse . PubSubPayload . (t, ) <$> handleTopic nid inboxed cached handlers t msg

subscribeHandler ::
       ( MonadReader env m
       , HasSubscribers env t, HasTopics env t, MonadIO m
       , Hashable t, Ord t)
    => NodeId
    -> Request ('PubSub 'Subscribe) (PubSubPayload t Timer)
    -> m (Response ('PubSub 'Subscribe) Status)
subscribeHandler nid (PubSubRequest (PubSubPayload (t, subTimer))) = do
  subs <- asks subscribers
  tops <- asks topics
  success <- liftIO $ newSubscriber nid subs tops subTimer t
  if success
    then return (PubSubResponse Ok)
    else return (PubSubResponse Error)

handleTopic ::
    (Eq t, Hashable t, Eq msg, Hashable msg, MonadIO m)
    => NodeId
    -> TVar (Inbox msg)
    -> TVar (Cache msg)
    -> TopicHandlers t msg
    -> t
    -> msg
    -> m msg
handleTopic nid inboxed cached (TopicHandlers handlers) t msg = do
    -- Add node to the inbox
    Inbox inbox' <- liftIO $ readTVarIO inboxed
    case inbox' ^. at msg of
        Just x -> liftIO $ atomically $ modifyTVar x (Set.insert nid)
        Nothing -> do
            def <- liftIO $ newTVarIO (Set.singleton nid)
            liftIO . atomically $
                modifyTVar
                    inboxed
                    (\(Inbox i) -> Inbox (i & at msg ?~ def))
    -- Return a response if already computed or add this message
    -- to cache map and wait for the result
    Cache cache' <- liftIO $ readTVarIO cached
    case cache' ^. at msg of
        Just x -> liftIO $ readMVar x
        Nothing -> do
            def <- liftIO newEmptyMVar
            liftIO . atomically $
                modifyTVar
                    cached
                    (\(Cache c) -> Cache (c & at msg ?~ def))
            case handlers ^. at t of
                Just (TopicHandler h) -> do
                    resp <- h msg
                    liftIO $ putMVar def resp
                    return resp
                Nothing -> error "Shouldn't reach here"
=======
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> Request ('PubSub 'Notify) (PubSubPayload t pmsg) -> m (Response ('PubSub 'Notify) Status)
notifyHandler nid (PubSubRequest payload@(PubSubPayload (_topic, msg))) = do
    $(logDebug) "Notify received handler invoked"
    resp <- handleTopic nid _topic msg
    case resp of
        Ok -> do
            $(logDebug) "handleTopic successful notifying subscribers"
            notify payload
        Error -> do
            $(logDebug) "handleTopic unsuccessful"
            return ()
    return (PubSubResponse resp)

publishHandler ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> Request ('PubSub 'Publish) (PubSubPayload t pmsg) -> m (Response ('PubSub 'Publish) Status)
publishHandler nid (PubSubRequest payload@(PubSubPayload (_topic, msg))) = do
    $(logDebug) "Publish received handler invoked"
    resp <- handleTopic nid _topic msg
    case resp of
        Ok -> do
            $(logDebug) "handleTopic successful notifying subscribers"
            notify payload
        Error -> do
            $(logDebug) "handleTopic unsuccessful"
            return ()
    return (PubSubResponse resp)

subscribeHandler ::
       (Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> Request ('PubSub 'Subscribe) (PubSubPayload t Timer) -> m (Response ('PubSub 'Subscribe) Status)
subscribeHandler nid (PubSubRequest (PubSubPayload (t, subTimer))) = do
    subs <- asks subscribers
    liftIO $ print (t)
    $(logDebug) "Subscribe received handler invoked"
    success <- liftIO $ newSubscriber nid subs subTimer t
    if success
        then return (PubSubResponse Ok)
        else return (PubSubResponse Error)

handleTopic :: (HasP2PEnv env m r t rmsg pmsg) => NodeId -> t -> pmsg -> m Status
handleTopic _nid topic msg = do
    hh <- asks psGlobalHandler
    resp <- hh topic msg
    return resp
>>>>>>> breaking out arivi-core from arivi
