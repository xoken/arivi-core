{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Arivi.P2P.PubSub.Types
    ( NodeTimer(..)
    , Subscribers(..)
    , Notifiers(..)
    , Status(..)
    , Timer
    , subscribersForTopic
    , notifiersForTopic
    , newSubscriber
    , newNotifier
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes
import Codec.Serialise (Serialise)
import Control.Applicative ()
import Control.Concurrent.STM
import Control.Lens ()
import Control.Monad.Trans
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock
import GHC.Generics (Generic)
import qualified StmContainers.Map as H

type Timer = Integer

data NodeTimer =
    NodeTimer
        { timerNodeId :: NodeId
        , timer :: UTCTime -- time here is current time added with the nominaldifftime in the message
        }
    deriving (Eq, Ord, Show, Generic, Serialise)

newtype Subscribers t =
    Subscribers (H.Map t (Set NodeId))

newtype Notifiers t =
    Notifiers (H.Map t (Set NodeId))

data Status
    = Ok
    | Error
    deriving (Eq, Ord, Show, Generic, Serialise)

subscribersForTopic :: (Eq t, Hashable t) => t -> Subscribers t -> IO (Set NodeId)
subscribersForTopic t (Subscribers subs) = do
    yy <- atomically $ (H.lookup t subs)
    case yy of
        Just x -> do
            atomically $ H.insert x t subs
            return x
        Nothing -> return Set.empty

notifiersForTopic :: (Eq t, Hashable t) => t -> Notifiers t -> IO (Set NodeId)
notifiersForTopic t (Notifiers notifs) = do
    yy <- liftIO $ atomically $ (H.lookup t notifs)
    case yy of
        Just x -> return x
        Nothing -> return Set.empty

newSubscriber ::
       (Ord t, Hashable t)
    => NodeId
    -> Subscribers t
    -> Integer -- Timer
    -> t
    -> IO Bool
newSubscriber nid (Subscribers subs) _ t = do
    yy <- liftIO $ atomically $ (H.lookup t subs)
    case yy of
        Just x -> do
            let nx = Set.insert nid x
            atomically $ H.insert nx t subs
            liftIO $ print ("modifying")
            return (True)
        Nothing -> do
            let z = Set.singleton nid
            atomically $ H.insert z t subs
            liftIO $ print ("insert-new")
            return True

newNotifier :: (Ord t, Hashable t) => NodeId -> Notifiers t -> t -> IO ()
newNotifier nid (Notifiers notifs) t = do
    yy <- liftIO $ atomically $ (H.lookup t notifs)
    case yy of
        Just x -> do
            let nx = Set.insert nid x
            atomically $ H.insert nx t notifs
    -- |Invariant this branch is never reached.
    -- 'initPubSub' should statically make empty
    -- sets for all topics in the map.
        Nothing -> return ()
