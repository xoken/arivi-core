{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Trans
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set
import qualified StmContainers.Map as H

-- Probably it's not worth it to put individual fields in TVar's instead
-- of the entire record.
data PubSubEnv t =
    PubSubEnv
        { pubSubTopics :: TVar (Set t)
        , pubSubSubscribers :: Subscribers t
        , pubSubNotifiers :: Notifiers t
        }

class (HasTopics env t, HasSubscribers env t, HasNotifiers env t) =>
      HasPubSubEnv env t
    where
    pubSubEnv :: env -> PubSubEnv t

type HasPubSub env t = (HasPubSubEnv env t, Eq t, Ord t, Hashable t, Serialise t)

mkPubSub :: [t] -> IO (PubSubEnv t)
mkPubSub _topicList = do
    PubSubEnv <$> newTVarIO (Set.empty) <*> (liftIO $ atomically $ Subscribers <$> H.new) <*>
        (liftIO $ atomically $ Notifiers <$> H.new)

instance HasTopics (PubSubEnv t) t where
    topics = pubSubTopics

instance HasSubscribers (PubSubEnv t) t where
    subscribers = pubSubSubscribers

instance HasNotifiers (PubSubEnv t) t where
    notifiers = pubSubNotifiers
