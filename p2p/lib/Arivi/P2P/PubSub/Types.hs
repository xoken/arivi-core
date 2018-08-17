{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Arivi.P2P.PubSub.Types
    ( Topic
    , Notifier
    , Watcher
    , TopicMessage
    , TopicHandler
    , ResponseCode(..)
    , MessageTypePubSub(..)
    , NodeTimer(..)
    , WatchersTable
    , NotifiersTable
    , TopicHandlerMap
    , MessageHashMap
    , SubscribeResponse(..)
    , Subscribe(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Data.ByteString.Char8                 as Char8 (ByteString)
import           Data.HashMap.Strict                   as HM
import           Data.SortedList
import           Data.Time.Clock
import           GHC.Generics                          (Generic)

type Topic = String

type Notifier = NodeTimer

type Watcher = NodeTimer

type TopicMessage = ByteString

type TopicHandler = (TopicMessage -> TopicMessage)

data ResponseCode
    = Error
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

-- convert the pico passed to diff time in functions, diff time is not serialisable
data MessageTypePubSub = 
    -- = Subscribe { topicId      :: Topic
    --             , messageTimer :: Integer }
      Notify { topicId      :: Topic
             , topicMessage :: TopicMessage }
    | Publish { topicId      :: Topic
              , topicMessage :: TopicMessage }
    -- | Response { responseCode :: ResponseCode
    --            , messageTimer :: Integer -- specify time duration as seconds
                -- }
    deriving (Eq, Ord, Show, Generic)

type Timer = Integer

data Subscribe t = Subscribe t Timer deriving(Eq, Ord, Generic, Serialise)
data SubscribeResponse t = SubscribeResponse t ResponseCode Timer deriving(Eq, Ord, Generic, Serialise)


instance Serialise MessageTypePubSub

data NodeTimer = NodeTimer
    { timerNodeId :: NodeId
    , timer       :: UTCTime -- time here is current time added with the nominaldifftime in the message
    }

instance Ord NodeTimer where
    x <= y = timer x <= timer y
    x > y = timer x > timer y
    x < y = timer x < timer y
    x >= y = timer x >= timer y

instance Eq NodeTimer where
    x == y = timerNodeId x == timerNodeId y

type WatchersTable = HM.HashMap Topic (TVar (SortedList Watcher))

type NotifiersTable
     = HM.HashMap Topic (TVar (SortedList Notifier), Int) -- might contain min no of peers
                                                -- and handler function here so dont use fst and snd in functions

type TopicHandlerMap --maps topic to the respective TopicHandler
     = HM.HashMap Topic TopicHandler

type MessageHashMap = HM.HashMap TopicMessage (TVar (Bool, [NodeId])) -- murmur hash of the TOpicMessage
