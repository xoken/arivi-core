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
    , subscribersForTopic
    , notifiersForTopic
    , notifiersForMessage
    , newSubscriber
    , newNotifier
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes

import           Codec.Serialise                       (Serialise)
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Lens
import           Data.Hashable
import           Data.HashMap.Strict                   as HM
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Time.Clock
import           GHC.Generics                          (Generic)

newtype TopicHandler msg =
    TopicHandler (forall m . msg -> m msg)

type Timer = Integer

data NodeTimer = NodeTimer
    { timerNodeId :: NodeId
    , timer :: UTCTime -- time here is current time added with the nominaldifftime in the message
    } deriving (Eq, Ord, Show, Generic, Serialise)

newtype Subscribers t = Subscribers (HM.HashMap t (TVar (Set NodeId)))

newtype Notifiers t = Notifiers (HM.HashMap t (TVar (Set NodeId)))

newtype Inbox msg = Inbox (HM.HashMap msg (TVar (Set NodeId)))

newtype Cache msg = Cache (HM.HashMap msg (MVar msg))

newtype TopicHandlers t msg = TopicHandlers (HM.HashMap t (TopicHandler msg))

data Status = Ok
            | Error
            deriving (Eq, Ord, Show, Generic, Serialise)

subscribersForTopic :: 
    ( Eq t
    , Hashable t
    )
    => t
    -> Subscribers t
    -> IO (Set NodeId)
subscribersForTopic t (Subscribers subs) =
    case subs ^. at t of
        Just x -> readTVarIO x
        Nothing -> return Set.empty

notifiersForTopic :: 
    ( Eq t
    , Hashable t
    )
    => t
    -> Notifiers t
    -> IO (Set NodeId)
notifiersForTopic t (Notifiers notifs) =
    case notifs ^. at t of
        Just x -> readTVarIO x
        Nothing -> return Set.empty

notifiersForMessage :: 
    ( Eq msg
    , Hashable msg
    , Eq t
    , Hashable t
    )
    => Inbox msg
    -> Subscribers t
    -> msg
    -> t
    -> IO (Set NodeId)
notifiersForMessage (Inbox inbox) subs msg t =
    case inbox ^. at msg of
        Just x -> liftA2 (Set.\\) (subscribersForTopic t subs) (readTVarIO x)
        -- |Invariant this branch is never reached.
        -- If no one sent a msg, you can't have a message
        -- to ask who sent it. Returning all subscribers.
        Nothing -> subscribersForTopic t subs

newSubscriber :: 
    (Ord t, Hashable t)
    => NodeId
    -> Subscribers t
    -> Set t
    -> Integer -- Timer
    -> t
    -> IO Bool
newSubscriber nid (Subscribers subs) topics _ t =
    if Set.member t topics
        then
            case subs ^. at t of
                Just x -> do
                    atomically $ modifyTVar x (Set.insert nid)
                    return True
                -- |Invariant this branch is never reached.
                -- 'initPubSub' should statically make empty
                -- sets for all topics in the map. Returning False.
                Nothing -> return False
        else return False   


newNotifier :: 
    (Ord t, Hashable t)
    => NodeId
    -> Notifiers t
    -> t
    -> IO ()
newNotifier nid (Notifiers notifs) t =
    case notifs ^. at t of
        Just x -> atomically $ modifyTVar x (Set.insert nid)
        -- |Invariant this branch is never reached.
        -- 'initPubSub' should statically make empty
        -- sets for all topics in the map.
        Nothing -> return ()