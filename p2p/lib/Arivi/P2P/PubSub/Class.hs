{-# LANGUAGE MultiParamTypeClasses #-}

module Arivi.P2P.PubSub.Class
    ( module Arivi.P2P.PubSub.Class
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import Arivi.P2P.PubSub.Types
       (Notifiers, Subscribers, TopicHandlerMap)

import Data.Hashable
import Data.Set (Set)

class HasSubscribers m t where
  subscribers :: m (Subscribers t)
  -- | subscribers who have not sent us this notify
  subscribers':: (Hashable msg) => msg -> t -> m (Set NodeId)
  subscribersForTopic :: t -> m (Set NodeId)

class HasNotifiers m t where
  notifiers :: m (Notifiers t)
  -- | Returns set of nodeIds for the given topic
  notifiersForTopic :: t -> m (Set NodeId)

class HasTopicHandlers m t msg where
  topicHandlers :: m (TopicHandlerMap t msg)