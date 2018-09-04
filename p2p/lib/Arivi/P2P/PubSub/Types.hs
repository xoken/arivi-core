{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Arivi.P2P.PubSub.Types
    ( Notify(..)
    , Publish(..)
    , Subscribe(..)
    , NodeTimer(..)
    , TopicHandler(..)
    , TopicHandlerMap(..)
    , Subscribers(..)
    , Notifiers(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.Types

import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Data.HashMap.Strict                   as HM
import           Data.Time.Clock
import           GHC.Generics                          (Generic)

newtype TopicHandler t msg =
    TopicHandler (PubSubPayload t msg -> PubSubPayload t msg)

data Publish t msg = Publish t msg
    deriving (Eq, Ord, Show, Generic, Serialise)

data Notify t msg = Notify t msg
    deriving (Eq, Ord, Show, Generic, Serialise)

data Subscribe t = Subscribe t Timer
    deriving (Eq, Ord, Show, Generic, Serialise)

type Timer = Integer

data NodeTimer = NodeTimer
    { timerNodeId :: NodeId
    , timer :: UTCTime -- time here is current time added with the nominaldifftime in the message
    } deriving (Eq, Ord, Show, Generic, Serialise)

newtype Subscribers t = Subscribers (HM.HashMap t (TVar [NodeId]))

newtype Notifiers t = Notifiers (HM.HashMap t (TVar [NodeId]))

newtype TopicHandlerMap t msg = TopicHandlerMap (HM.HashMap t (TopicHandler t msg))
