module Arivi.P2P.PubSub.Types
    ( Topic
    , Notifier
    , Watcher
    , TopicMessage
    , TopicHandler
    , ResponseCode
    , MessageTypePubSub(..)
    , NodeTimer(..)
    , WatchersTable
    , NotifiersTable
    , TopicHandlerMap
    , MessageHashMap
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar

import           Data.ByteString.Char8                 as Char8 (ByteString)
import           Data.HashMap.Strict                   as HM
import qualified Data.Map.Strict                       as Map
import           Data.SortedList
import           Data.Time.Clock

type Topic = String

type Notifier = NodeTimer

type Watcher = NodeTimer

type TopicMessage = ByteString

type TopicHandler = (TopicMessage -> TopicMessage)

type ResponseCode = Int

data MessageTypePubSub
    = Subscribe { topicId      :: Topic
                , messageTimer :: NominalDiffTime }
    | Notify { topicId      :: Topic
             , topicMessage :: TopicMessage }
    | Publish { topicId      :: Topic
              , topicMessage :: TopicMessage }
    | Response { responseCode :: ResponseCode
               , messageTimer :: NominalDiffTime }

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

type WatchersTable = HM.HashMap Topic (TVar (SortedList Watcher)) -- need to use sorted list

type NotifiersTable
     = HM.HashMap Topic (TVar (SortedList Notifier), Int) -- need to use sorted list, might contain min no of peers
                                                -- and handler function here so dont use fst and snd in functions

type TopicHandlerMap --maps topic to the respective TopicHandler
     = HM.HashMap Topic TopicHandler

type MessageHashMap = HM.HashMap TopicMessage (TVar (Bool, [NodeId])) -- murmur hash of the TOpicMessage
