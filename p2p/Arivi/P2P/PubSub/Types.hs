module Arivi.P2P.PubSub.Types
    (
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes

import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar

import           Data.Digest.Murmur32
import           Data.HashMap.Strict                   as HM
import           Data.Time.Clock

type Topic = String

type Notifier = NodeTimer

type Watcher = NodeTimer

type TopicMessage = ByteString

type TopicHandler a = (a -> TopicMessage)

type ResponseCode = Int

data MessageTypePubSub
    = Subscribe { topicId :: TopicId
                , timer   :: NominalDiffTime }
    | Notify { topicId      :: TopicId
             , topicMessage :: TopicMessage }
    | Publish { topicId      :: TopicId
              , topicMessage :: TopicMessage }
    | Response { responseCode :: ResponseCode
               , timer        :: NominalDiffTime }

data NodeTimer = NodeTimer
    { nodeId :: NodeId
    , timer  :: UTCTime -- time here is current time added with the nominaldifftime in the message
    }

instance Ord NodeTimer where
    x <= y = timer x <= timer y
    x > y = timer x > timer y
    x < y = timer x < timer y
    x >= y = timer x >= timer y

instance Eq NodeTimer where
    x == y = nodeId x == nodeId y

type WatchersTable = HM.Hashmap Topic TVar [Watchers] -- need to use sorted list

type NotifiersTable
     = HM.Hashmap Topic (TVar [Notifiers], Int) -- need to use sorted list, might contain min no of peers and handler function here so dont use fst and snd in functions

type TopicHandlerMap --maps topic to the respective TopicHandler
     = HM.Hashmap Topic TopicHandler

type MessageHashMap = HM.HashMap Hash32 TVar (Bool, [NodeId]) -- murmur hash of the TOpicMessage
