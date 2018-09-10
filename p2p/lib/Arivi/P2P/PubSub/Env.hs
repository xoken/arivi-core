{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |The instances should be modified, ideally we'd want them to work
-- given a PubSubEnv, and they do work now. But we want them in a way
-- to specify instances for different f's in PubSubEnv, essentially
-- we don't want the MonadReader constraint on the instance defintion.

module Arivi.P2P.PubSub.Env
    ( PubSubEnv(..)
    ) where

import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import Data.Set (Set)
import qualified Data.Set as Set

-- Probably it's not worth it to put individual fields in TVar's instead
-- of the entire record.
data PubSubEnv f t msg = PubSubEnv
    { pubSubTopics :: Set t
    , pubSubSubscribers :: Subscribers f t
    , pubSubNotifiers :: Notifiers f t
    , pubSubInbox :: f (Inbox f msg)
    , pubSubCache :: f (Cache MVar msg)
    , pubSubHandlers :: TopicHandlers t msg
    }

class HasPubSubEnv m t msg | m -> t msg where
    pubSubEnv :: m (PubSubEnv f t msg)

instance ( Ord t
         , Eq t
         , Hashable t
         , Eq msg
         , Hashable msg
         , HasPubSubEnv m t msg
         , MonadIO m
         ) =>
         HasSubscribers m t msg where
    subscribers t = do
        Subscribers subs <- pubSubSubscribers <$> pubSubEnv
        case subs ^. at t of
            Just x -> liftIO $ readTVarIO x
            Nothing -> return Set.empty
    subscribers' msg t = do
        Inbox inbox <- join $ liftIO . readTVarIO <$> (pubSubInbox <$> pubSubEnv)
        case inbox ^. at msg of
            Just x -> liftA2 (Set.\\) (subscribers t) (liftIO $ readTVarIO x)
            -- |Invariant this branch is never reached.
            -- If no one sent a msg, you can't have a message
            -- to ask who sent it. Returning all subscribers.
            Nothing -> liftA2 (Set.\\) (subscribers t) (pure Set.empty)
    newSubscriber nid _ t = do
        topics <- pubSubTopics <$> pubSubEnv
        if Set.member t topics
            then do
                Subscribers subs <- pubSubSubscribers <$> pubSubEnv
                case subs ^. at t of
                    Just x -> do
                        liftIO . atomically $ modifyTVar x (Set.insert nid)
                        return True
                    -- |Invariant this branch is never reached.
                    -- 'initPubSub' should statically make empty
                    -- sets for all topics in the map. Returning False.
                    Nothing -> return False
            else return False

instance (Eq t, Hashable t, HasPubSubEnv m t msg, MonadIO m) =>
         HasNotifiers m t where
    notifiers t = do
        Notifiers notifs <- pubSubNotifiers <$> pubSubEnv
        case notifs ^. at t of
            Just x -> liftIO $ readTVarIO x
            Nothing -> return Set.empty
    newNotifier nid t = do
        Notifiers notifs <- pubSubNotifiers <$> pubSubEnv
        case notifs ^. at t of
            Just x -> liftIO . atomically $ modifyTVar x (Set.insert nid)
            -- |Invariant this branch is never reached.
            -- 'initPubSub' should statically make empty
            -- sets for all topics in the map.
            Nothing -> return ()

instance ( Eq t
         , Hashable t
         , Eq msg
         , Hashable msg
         , HasPubSubEnv m t msg
         , MonadIO m
         ) =>
         HasTopicHandlers m t msg where
    handleTopic nid t msg = do
        -- Add node to the inbox
        inboxed <- pubSubInbox <$> pubSubEnv
        Inbox inbox <- liftIO $ readTVarIO inboxed
        case inbox ^. at msg of
            Just x -> liftIO . atomically $ modifyTVar x (Set.insert nid)
            Nothing -> do
                def <- liftIO $ newTVarIO (Set.singleton nid)
                liftIO . atomically $
                    modifyTVar
                        inboxed
                        (\(Inbox i) -> Inbox (i & at msg ?~ def))
        -- Return a response if already computed or add this message
        -- to cache map and wait for the result
        cached <- pubSubCache <$> pubSubEnv
        Cache cache <- liftIO $ readTVarIO cached
        case cache ^. at msg of
            Just x -> liftIO $ readMVar x
            Nothing -> do
                def <- liftIO newEmptyMVar
                liftIO . atomically $
                    modifyTVar
                        cached
                        (\(Cache c) -> Cache (c & at msg ?~ def))
                TopicHandlers handlers <- pubSubHandlers <$> pubSubEnv
                case handlers ^. at t of
                    Just (TopicHandler h) -> do
                        resp <- h msg
                        liftIO $ putMVar def resp
                        return resp
                    Nothing -> error "Shouldn't reach here"
