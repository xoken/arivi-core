module Arivi.P2P.PubSub.Functions
    ( publishTopic
    ) where

import           Arivi.P2P.MessageHandler.Handler      (sendRequest)
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        NodeId)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Types
import           Codec.Serialise                       (serialise)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class                (liftIO)
-- import           Control.Monad.STM

import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import qualified Data.HashMap.Strict                   as HM
-- import qualified Data.List                             as List
import           Data.Maybe
import           Data.SortedList
-- import           Data.Time.Clock

-- | Called by each service to register its Topics. Creates entries in TopicMap
-- the TopicHandler passed takes a topicmessage and returns a topic
-- if topicmessage is returned it is sent to all watchers
-- it can also return exception if the block is valid or invalidaccordingly peers are judged
-- #registerTopic :: (HasP2PEnv m) => [(Topic, TopicHandler, Int)] -> m ()
-- | called by a service when some content created by it needs to be broadcasted to the network ie notifiers as well as subscribers
-- | spawns a thread with the parameter TVar TopicMap and checks expired notifiers and possible resubscribe to the peer based on rep. also checks if min number if satisfied and add more peers accordingly by calling send options which takes peer from kad
-- #maintainNotifiers :: (HasP2PEnv m) => m ()-- -- | called by a service to read the TopicMessage TQueue
-- readTopic :: (HasP2PEnv m) => TopicId -> m TopicMessage
-- -- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers. only sent to Subscribers minus nodes in MessageHashMap
-- notifyTopic :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()\
-- -- | called in case of a unvalid block and used for peer rep based on MessageHashMap
-- flagMessage :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()
publishTopic :: (HasP2PEnv m) => Topic -> TopicMessage -> m ()
publishTopic messageTopic publishMessage = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    -- notifierTableTVar <- getNotifiersTableP2PEnv
    -- notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currWatcherListTvarMaybe = HM.lookup messageTopic watcherMap
    -- let currNotifierListTvarMaybe = HM.lookup messageTopic notifierMap
    currWatcherSortedList <-
        liftIO $ readTVarIO $ fromJust currWatcherListTvarMaybe
    -- TODO Remove if not needed
    -- currNotifierSortedList <-
    --     liftIO $ readTVarIO $ fromJust currNotifierListTvarMaybe
    let currWatcherList = fromSortedList currWatcherSortedList
        -- TODO Remove if not needed
    -- let currNotifierList = fromSortedList currNotifierSortedList
    -- let combinedList = currWatcherList `List.union` currNotifierList
    let message =
            Publish {topicId = messageTopic, topicMessage = publishMessage}
    let serializedMessage = serialise message
    sendPublishMessage currWatcherList serializedMessage

sendPublishMessage :: (HasP2PEnv m) => [NodeTimer] -> Lazy.ByteString -> m ()
sendPublishMessage [] _ = return ()
sendPublishMessage (recievingPeer:peerList) message = do
    let recievingPeerNodeId = timerNodeId recievingPeer
    _ <- LAsync.async (sendPublishMessageToPeer recievingPeerNodeId message)
    sendPublishMessage peerList message

-- will have to do exception handling based on the response of sendRequest
sendPublishMessageToPeer :: (HasP2PEnv m) => NodeId -> Lazy.ByteString -> m ()
sendPublishMessageToPeer recievingPeerNodeId message = do
    _ <- sendRequest recievingPeerNodeId PubSub message -- wrapper written execptions can be handled here and  integrity of the response can be checked
    return ()

-- TODO Remove if not needed

-- maintainWatchers :: (HasP2PEnv m) => m ()
-- maintainWatchers = do
--     watcherTableTVar <- getWatcherTableP2PEnv
--     watcherMap <- liftIO $ readTVarIO watcherTableTVar
--     let topicIds = HM.keys watcherMap
--     _ <- LAsync.async (maintainWatchersHelper watcherMap topicIds)
--     return ()

-- TODO Remove if not needed

-- maintainWatchersHelper :: (HasP2PEnv m) => WatchersTable -> [Topic] -> m ()
-- maintainWatchersHelper _ [] = return ()
-- maintainWatchersHelper watcherMap (topic:topicIdList) = do
--     let currListTvarMaybe = HM.lookup topic watcherMap
--     let currListTvar = fromJust currListTvarMaybe
--     currTime <- liftIO getCurrentTime
--     liftIO $
--         atomically
--             (do currSortedList <- readTVar currListTvar
--                 let currList = fromSortedList currSortedList
--                 let newList = checkWatchers currList currTime
--                 let newSortedList = toSortedList newList
--                 writeTVar currListTvar newSortedList)
--     maintainWatchersHelper watcherMap topicIdList

-- will take the list of watchers for each topic and check their validity

-- TODO Remove if not needed

-- checkWatchers :: [Watcher] -> UTCTime -> [Watcher]
-- checkWatchers [] _ = []
-- checkWatchers (currWatcher:watcherList) currentTime =
--     if timer currWatcher < currentTime
--         then [] ++ checkWatchers watcherList currentTime
--         else currWatcher : watcherList
-- checkWatchers :: [Watcher] -> IO [Watcher]
-- checkWatchers [] = return []
-- checkWatchers (currWatcher : watcherList) =
-- 	do
-- 		let watcherTime = timer currWatcher
-- 		currentTime <- getCurrentTime
-- 		if (currentTime > watcherTime) -- then the watcher has expired
-- 			then
-- 				do
-- 					nextWatcher <- checkWatchers watcherList
-- 					return nextWatcher
-- 			else
-- 				return (currWatcher : watcherList)
