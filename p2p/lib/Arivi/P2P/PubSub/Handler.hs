{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.PubSub.Handler
    ( pubSubHandler
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
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
pubSubHandler nid Subscribe req = serialise <$> subscribeHandler nid (deserialise req)
pubSubHandler nid Publish req = serialise <$> publishHandler nid (deserialise req)
pubSubHandler nid Notify req = serialise <$> notifyHandler nid (deserialise req)

notifyHandler ::
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
