module Arivi.P2P.PubSub.Functions
    ( publishTopic
    , maintainWatchers
    , updateDynamicResourceToPeerMap -- remove form exported functions later
    , notifyTopic
    ) where

import           Arivi.P2P.MessageHandler.Handler      (sendRequest)
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        NodeId)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types                   (ResourceId)
import           Arivi.Utils.Logging
import           Codec.Serialise                       (serialise)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM

import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List                             as List
import           Data.Maybe
import           Data.SortedList
import           Data.Time.Clock

-- | Called by each service to register its Topics. Creates entries in TopicMap
-- the TopicHandler passed takes a topicmessage and returns a topic
-- if topicmessage is returned it is sent to all watchers
-- it can also return exception if the block is valid or invalidaccordingly peers are judged
-- #registerTopic :: (HasP2PEnv m) => [(Topic, TopicHandler, Int)] -> m ()
-- | spawns a thread with the parameter TVar TopicMap and checks expired notifiers and possible resubscribe to the peer based on rep. also checks if min number if satisfied and add more peers accordingly by calling send options which takes peer from kad
-- #maintainNotifiers :: (HasP2PEnv m) => m ()-- -- | called by a service to read the TopicMessage TQueue
-- readTopic :: (HasP2PEnv m) => TopicId -> m TopicMessage
-- -- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers. only sent to Subscribers minus nodes in MessageHashMap
-- notifyTopic :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()
-- -- | called in case of a unvalid block and used for peer rep based on MessageHashMap
-- flagMessage :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()
-- | called by a service when some content created by it needs to be broad-casted to the network i.e. notifiers as well as subscribers
publishTopic :: (HasP2PEnv m, HasLogging m) => Topic -> TopicMessage -> m ()
publishTopic messageTopic publishMessage = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currWatcherListTvarMaybe = HM.lookup messageTopic watcherMap
    let currNotifierListTvarMaybe = HM.lookup messageTopic notifierMap
    currWatcherSortedList <-
        liftIO $ readTVarIO $ fromJust currWatcherListTvarMaybe
    currNotifierSortedList <-
        liftIO $ readTVarIO $ fromJust currNotifierListTvarMaybe
    let currWatcherList = fromSortedList currWatcherSortedList
    let currNotifierList = fromSortedList currNotifierSortedList
    let combinedList = currWatcherList `List.union` currNotifierList
    let nodeIdList = List.map timerNodeId combinedList
    let message =
            Publish {topicId = messageTopic, topicMessage = publishMessage}
    let serializedMessage = serialise message
    sendPubSubMessage nodeIdList serializedMessage

sendPubSubMessage ::
       (HasP2PEnv m, HasLogging m) => [NodeId] -> Lazy.ByteString -> m ()
sendPubSubMessage [] _ = return ()
sendPubSubMessage (recievingPeerNodeId:peerList) message = do
    _ <- LAsync.async (sendPubSubMessageToPeer recievingPeerNodeId message)
    sendPubSubMessage peerList message

-- will have to do exception handling based on the response of sendRequest
sendPubSubMessageToPeer ::
       (HasP2PEnv m, HasLogging m) => NodeId -> Lazy.ByteString -> m ()
sendPubSubMessageToPeer recievingPeerNodeId message = do
    _ <- sendRequest recievingPeerNodeId PubSub message -- wrapper written execptions can be handled here and  integrity of the response can be checked
    return ()

maintainWatchers :: (HasP2PEnv m, HasLogging m) => m ()
maintainWatchers = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let topicIds = HM.keys watcherMap
    _ <- LAsync.async (maintainWatchersHelper watcherMap topicIds)
    return ()

maintainWatchersHelper ::
       (HasP2PEnv m, HasLogging m) => WatchersTable -> [Topic] -> m ()
maintainWatchersHelper _ [] = return ()
maintainWatchersHelper watcherMap (topic:topicIdList) = do
    let currListTvarMaybe = HM.lookup topic watcherMap
    let currListTvar = fromJust currListTvarMaybe
    currTime <- liftIO getCurrentTime
    liftIO $
        atomically
            (do currSortedList <- readTVar currListTvar
                let currList = fromSortedList currSortedList
                let newList = checkWatchers currList currTime
                let newSortedList = toSortedList newList
                writeTVar currListTvar newSortedList)
    maintainWatchersHelper watcherMap topicIdList

-- will take the list of watchers for each topic and check their validity
checkWatchers :: [Watcher] -> UTCTime -> [Watcher]
checkWatchers [] _ = []
checkWatchers (currWatcher:watcherList) currentTime =
    if timer currWatcher < currentTime
        then [] ++ checkWatchers watcherList currentTime
        else currWatcher : watcherList

-- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers.
-- Only sent to Subscribers minus nodes in MessageHashMap
notifyTopic :: (HasP2PEnv m, HasLogging m) => Topic -> TopicMessage -> m ()
notifyTopic mTopic mTopicMessage = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let listOfWatchersForTopic = HM.lookup mTopic watcherMap
    if isNothing listOfWatchersForTopic
        then return () -- what to do if no watchers are present for a given topic
        else do
            let notifyMessage =
                    Notify {topicId = mTopic, topicMessage = mTopicMessage}
            let noteMessageByteString = serialise notifyMessage
            currWatcherSortedList <-
                liftIO $ readTVarIO $ fromJust listOfWatchersForTopic
            let watcherList = fromSortedList currWatcherSortedList
            let watcherNodeIdList = List.map timerNodeId watcherList
            messageHashMapTvar <- getMessageHashMapP2PEnv
            messageHashMap <- liftIO $ readTVarIO messageHashMapTvar
            let currTuple = HM.lookup mTopicMessage messageHashMap
                    -- need to add an exception for failed sending of pubsub message
            if isNothing currTuple
                             -- there are no nodes for this message in the MessageHashMap so send to all the watchers
                then sendPubSubMessage watcherNodeIdList noteMessageByteString
                else do
                    messageMapTuple <- liftIO $ readTVarIO $ fromJust currTuple
                    let messageNodeIdList = snd messageMapTuple
                    let finalListOfWatchers =
                            watcherNodeIdList List.\\ messageNodeIdList
                    sendPubSubMessage finalListOfWatchers noteMessageByteString

-- | used by pubsub to update the dynamic resource to peer mapping when it receives a notify message for a particular dynamic resource
updateDynamicResourceToPeerMap ::
       (HasP2PEnv m, HasLogging m) => ResourceId -> NodeId -> m ()
updateDynamicResourceToPeerMap resID nodeID = do
    dynamicResourceToPeerMapTVar <- getDynamicResourceToPeerMap
    liftIO $
        atomically
            (do dynamicResourceToPeerMap <-
                    readTVar dynamicResourceToPeerMapTVar
                let currentEntry = HM.lookup resID dynamicResourceToPeerMap
                if isNothing currentEntry
                    then do
                        newNodeTVar <- newTVar [nodeID]
                        let modifiedMap =
                                HM.insert
                                    resID
                                    newNodeTVar
                                    dynamicResourceToPeerMap
                        writeTVar dynamicResourceToPeerMapTVar modifiedMap
                    else do
                        currNodeList <- readTVar $ fromJust currentEntry
                        let newNodeList = currNodeList ++ [nodeID]
                        writeTVar (fromJust currentEntry) newNodeList)
