{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Arivi.P2P.PubSub.Handler
    ( pubSubHandler
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import Arivi.P2P.Types
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types (Status(..), Timer)

import Codec.Serialise
import Data.ByteString.Lazy

-- |Handler functions might never halt, they should be run
-- in a separate thread.

pubSubHandler ::
       ( HasTopicHandlers m t msg
       , HasSubscribers m t msg
       , Serialise t
       , Serialise msg
       , Monad m
       )
    => NodeId
    -> PubSub
    -> ByteString
    -> m ByteString
pubSubHandler nid Subscribe req = serialise <$> subscribeHandler nid (deserialise req)
pubSubHandler nid Publish req = serialise <$> publishHandler nid (deserialise req)
pubSubHandler nid Notify req = serialise <$> notifyHandler nid (deserialise req)

notifyHandler ::
       (HasTopicHandlers m t msg, Monad m)
    => NodeId
    -> Request ('PubSub 'Notify) (PubSubPayload t msg)
    -> m (Response ('PubSub 'Notify) (PubSubPayload t msg))
notifyHandler nid (PubSubRequest (PubSubPayload (t, msg))) =
    (PubSubResponse . PubSubPayload . (t, )) <$> handleTopic nid t msg

publishHandler ::
       (HasTopicHandlers m t msg, Monad m)
    => NodeId
    -> Request ('PubSub 'Publish) (PubSubPayload t msg)
    -> m (Response ('PubSub 'Publish) (PubSubPayload t msg))
publishHandler nid (PubSubRequest (PubSubPayload (t, msg))) =
    (PubSubResponse . PubSubPayload . (t, )) <$> handleTopic nid t msg

subscribeHandler ::
       (HasSubscribers m t msg, Monad m)
    => NodeId
    -> Request ('PubSub 'Subscribe) (PubSubPayload t Timer)
    -> m (Response ('PubSub 'Subscribe) Status)
subscribeHandler nid (PubSubRequest (PubSubPayload (t, timer))) = do
  success <- newSubscriber nid timer t
  if success
    then return (PubSubResponse Ok)
    else return (PubSubResponse Error)
