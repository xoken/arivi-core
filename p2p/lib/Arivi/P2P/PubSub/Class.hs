{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arivi.P2P.PubSub.Class
    ( module Arivi.P2P.PubSub.Class
    ) where

import Arivi.P2P.PubSub.Types

import Control.Concurrent.STM.TVar (TVar)
import Data.Set (Set)

class HasTopics env t | env -> t where
    topics :: env -> TVar (Set t)

class HasSubscribers env t | env -> t where
    subscribers :: env -> Subscribers t

class HasNotifiers env t | env -> t where
    notifiers :: env -> Notifiers t
