{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.PubSub.Types
    ( MessageHashMap
    , MessageTypePubSub(..)
    , NodeTimer(..)
    , Notifier
    , NotifiersTable
    , ResponseCode(..)
    , Topic
    , TopicHandler
    , TopicHandlerMap
    , TopicMessage(..)
    , Watcher
    , WatchersTable
    , ErrorType(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Data.ByteString.Char8                 as Char8 (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict                   as HM
import           Data.SortedList
import           Data.Time.Clock
import           GHC.Generics                          (Generic)

data NodeTimer = NodeTimer
    { timerNodeId :: NodeId
    , timer       :: UTCTime -- ^ Time here is current time added with the
                             -- ^ nominal difftime in the message
    }

instance Ord NodeTimer where
    x <= y = timer x <= timer y
    x > y = timer x > timer y
    x < y = timer x < timer y
    x >= y = timer x >= timer y

instance Eq NodeTimer where
    x == y = timerNodeId x == timerNodeId y

newtype Topic =
    Topic ByteString
    deriving (Eq, Ord, Show, Generic)

instance Serialise Topic

instance Hashable Topic

type Notifier = NodeTimer

type Watcher = NodeTimer

newtype TopicMessage =
    TopicMessage ByteString
    deriving (Eq, Ord, Show, Generic)

instance Serialise TopicMessage

instance Hashable TopicMessage

type TopicHandler = (TopicMessage -> Either ResponseCode TopicMessage)

data ResponseCode
    = Error { errCode :: ErrorType }
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

data ErrorType
    = DeserialiseError
    | InvalidTopicError
    | DuplicateMessage
    | TopicHandlerNotRegistered
    | InvalidNotifierError
    | InvalidResponseError
    | Unknown
    deriving (Eq, Ord, Show, Generic)

instance Serialise ErrorType

data MessageTypePubSub
    = Subscribe { nodeId       :: NodeId
                , topicId      :: Topic
                , messageTimer :: Integer }
    | Notify { nodeId       :: NodeId
             , topicId      :: Topic
             , topicMessage :: TopicMessage }
    | Publish { nodeId       :: NodeId
              , topicId      :: Topic
              , topicMessage :: TopicMessage }
    | Response { responseCode :: ResponseCode
               , messageTimer :: Integer -- ^ Specify time duration as seconds
                }
    deriving (Eq, Ord, Show, Generic)

-- type Timer = Integer
-- data Subscribe t = Subscribe t Timer deriving(Eq, Ord, Generic, Serialise)
-- data SubscribeResponse t = SubscribeResponse t ResponseCode Timer deriving(Eq, Ord, Generic, Serialise)
instance Serialise MessageTypePubSub

type WatchersTable = HM.HashMap Topic (TVar (SortedList Watcher))

-- | Might contain min no of peers and handler function here so dont use fst
-- and snd in functions
type NotifiersTable = HM.HashMap Topic (TVar (SortedList Notifier), Int)

-- | Maps topic to the respective TopicHandler
type TopicHandlerMap = HM.HashMap Topic TopicHandler

-- | Murmur hash of the TOpicMessage
type MessageHashMap = HM.HashMap TopicMessage (TVar [NodeId])
