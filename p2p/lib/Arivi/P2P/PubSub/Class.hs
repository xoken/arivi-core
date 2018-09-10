{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Arivi.P2P.PubSub.Class
    ( module Arivi.P2P.PubSub.Class
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)

import Codec.Serialise
import Data.Set (Set)

class HasSubscribers m t msg | m -> t msg where
    subscribers :: t -> m (Set NodeId)
    -- |subscribers who have not sent us this notify
    subscribers' :: msg -> t -> m (Set NodeId)
    newSubscriber :: NodeId -> Integer -> t -> m Bool

class HasNotifiers m t where
    notifiers :: t -> m (Set NodeId)
    newNotifier :: NodeId -> t -> m ()

class HasTopicHandlers m t msg | m -> t msg where
    handleTopic :: NodeId -> t -> msg -> m msg

type HasPubSub m t msg
     = ( HasSubscribers m t msg
       , HasNotifiers m t
       , HasTopicHandlers m t msg
       , Serialise t
       , Serialise msg
       , Monad m)
