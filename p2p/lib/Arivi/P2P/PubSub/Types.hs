{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Arivi.P2P.PubSub.Types
    ( NodeTimer(..)
    , TopicHandler(..)
    , Subscribers(..)
    , Notifiers(..)
    , Inbox(..)
    , Cache(..)
    , TopicHandlers(..)
    , Status(..)
    , Timer
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes

import           Codec.Serialise                       (Serialise)
import           Data.HashMap.Strict                   as HM
import           Data.Set                              (Set)
import           Data.Time.Clock
import           GHC.Generics                          (Generic)

newtype TopicHandler msg =
    TopicHandler (forall m . msg -> m msg)

type Timer = Integer

data NodeTimer = NodeTimer
    { timerNodeId :: NodeId
    , timer :: UTCTime -- time here is current time added with the nominaldifftime in the message
    } deriving (Eq, Ord, Show, Generic, Serialise)

newtype Subscribers f t = Subscribers (HM.HashMap t (f (Set NodeId)))

newtype Notifiers f t = Notifiers (HM.HashMap t (f (Set NodeId)))

newtype Inbox f msg = Inbox (HM.HashMap msg (f (Set NodeId)))

newtype Cache f msg = Cache (HM.HashMap msg (f msg))

newtype TopicHandlers t msg = TopicHandlers (HM.HashMap t (TopicHandler msg))

data Status = Ok
            | Error
            deriving (Eq, Ord, Show, Generic, Serialise)
