{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}

-- |The instances should be modified, ideally we'd want them to work
-- given a PubSubEnv, and they do work now. But we want them in a way
-- to specify instances for different f's in PubSubEnv, essentially
-- we don't want the MonadReader constraint on the instance defintion.

module Arivi.P2P.PubSub.Env
    ( PubSubEnv(..)
    , mkPubSub
    , HasPubSub
    , HasPubSubEnv(..)
    ) where

import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types

import Codec.Serialise
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Data.HashMap.Strict as HM
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set

-- Probably it's not worth it to put individual fields in TVar's instead
-- of the entire record.
data PubSubEnv t msg = PubSubEnv
    { pubSubTopics :: Set t
    , pubSubSubscribers :: Subscribers t
    , pubSubNotifiers :: Notifiers t
    , pubSubInbox :: TVar (Inbox msg)
    , pubSubCache :: TVar (Cache msg)
    , pubSubHandlers :: TopicHandlers t msg
    }

class (HasTopics env t, HasSubscribers env t, HasNotifiers env t, HasInbox env msg, HasCache env msg, HasTopicHandlers env t msg) => HasPubSubEnv env t msg where
        pubSubEnv :: env -> PubSubEnv t msg

-- type HasPubSubEnv env t msg = (HasTopics env t, HasSubscribers env t, HasNotifiers env t, HasInbox env msg, HasCache env msg, HasTopicHandlers env t msg)

type HasPubSub env t msg
    = ( HasPubSubEnv env t msg
      , Eq t, Ord t, Hashable t, Serialise t
      , Eq msg, Hashable msg, Serialise msg
      )


mkPubSub :: IO (PubSubEnv t msg)
mkPubSub =
    PubSubEnv <$> pure Set.empty
              <*> pure (Subscribers HM.empty)
              <*> pure (Notifiers HM.empty)
              <*> newTVarIO (Inbox HM.empty)
              <*> newTVarIO (Cache HM.empty)
              <*> pure (TopicHandlers HM.empty)

instance HasTopics (PubSubEnv t msg) t where
    topics = pubSubTopics

instance HasSubscribers (PubSubEnv t msg) t where
    subscribers = pubSubSubscribers

instance HasNotifiers (PubSubEnv t msg) t where
    notifiers = pubSubNotifiers

instance HasInbox (PubSubEnv t msg) msg where
    inbox = pubSubInbox

instance HasCache (PubSubEnv t msg) msg where
    cache = pubSubCache

instance HasTopicHandlers (PubSubEnv t msg) t msg where
    topicHandlers = pubSubHandlers
