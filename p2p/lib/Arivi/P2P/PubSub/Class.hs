{-# LANGUAGE MultiParamTypeClasses #-}

module Arivi.P2P.PubSub.Class
    ( module Arivi.P2P.PubSub.Class
    ) where

import Arivi.P2P.PubSub.Types
       (Notifiers, Subscribers, TopicHandlerMap)

class HasSubscribers m t where
  subscribers :: m (Subscribers t)

class HasNotifiers m t where
  notifiers :: m (Notifiers t)

class HasTopicHandlers m t msg where
  topicHandlers :: m (TopicHandlerMap t msg)
