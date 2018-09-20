{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TemplateHaskell #-}

module Arivi.P2P.PubSub.Handler
    ( pubSubHandler
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import Arivi.P2P.Types
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.PubSub.Notify

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens
import Control.Monad.Reader
import Data.Hashable
import Data.ByteString.Lazy
import qualified Data.Set as Set
import qualified Data.Text  as T
import Control.Monad.Logger (logDebug)

-- |Handler functions might never halt, they should be run
-- in a separate thread.

pubSubHandler ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => NodeId
    -> PubSub
    -> ByteString
    -> m ByteString
pubSubHandler nid Subscribe req = serialise <$> subscribeHandler nid (deserialise req)
pubSubHandler nid Publish req = serialise <$> publishHandler nid (deserialise req)
pubSubHandler nid Notify req = serialise <$> notifyHandler nid (deserialise req)

notifyHandler ::
       ( HasP2PEnv env m r t rmsg pmsg)
    => NodeId
    -> Request ('PubSub 'Notify) (PubSubPayload t pmsg)
    -> m (Response ('PubSub 'Notify) Status)
notifyHandler nid (PubSubRequest payload@(PubSubPayload (t, msg))) = do
    $(logDebug) $ T.pack ("Notify received handler invoked")
    inboxed <- asks inbox
    cached <- asks cache
    h <- asks topicHandlers
    resp <- handleTopic nid inboxed cached h t msg
    case resp of
        Ok -> do
            $(logDebug) $ T.pack ("handleTopic successful notifying subscribers")
            notify payload
        Error -> do
            $(logDebug) $ T.pack ("handleTopic unsuccessful")
            return ()
    return (PubSubResponse resp)

publishHandler ::
       ( HasP2PEnv env m r t rmsg pmsg)
    => NodeId
    -> Request ('PubSub 'Publish) (PubSubPayload t pmsg)
    -> m (Response ('PubSub 'Publish) Status)
publishHandler nid (PubSubRequest payload@(PubSubPayload (t, msg))) = do
    $(logDebug) $ T.pack ("Publish received handler invoked")
    inboxed <- asks inbox
    cached <- asks cache
    h <- asks topicHandlers
    resp <- handleTopic nid inboxed cached h t msg
    case resp of
        Ok -> do
            $(logDebug) $ T.pack ("handleTopic successful notifying subscribers")
            notify payload
        Error -> do
            $(logDebug) $ T.pack ("handleTopic unsuccessful")
            return ()
    return (PubSubResponse resp)

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
    -> m Status
handleTopic nid inboxed cached (TopicHandlers hs) t msg = do
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
            case hs ^. at t of
                Just (TopicHandler h) -> do
                    let resp = h msg
                    liftIO $ putMVar def resp
                    return resp
                Nothing -> error "Shouldn't reach here"
