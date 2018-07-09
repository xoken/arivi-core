module Arivi.P2P.PubSub.Functions
    ( publishTopic
    , maintainWatchers
    , updateDynamicResourceToPeerMap -- remove form exported functions later
    , notifyTopic
    , registerTopic
    , maintainNotifiers
    ) where

import           Arivi.P2P.MessageHandler.Handler      (sendRequest)
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        NodeId)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types                   (ResourceHandler,
                                                        ResourceId)
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialise, serialise)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM

import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List                             as List
import           Data.Maybe
import           Data.SortedList
import           Data.Time.Clock

-- | called by a service to read the TopicMessage TQueue
-- readTopic :: (HasP2PEnv m) => TopicId -> m TopicMessage
-- -- | called in case of a invalid block and used for peer rep based on MessageHashMap
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
        liftIO $ readTVarIO $ fst $ fromJust currNotifierListTvarMaybe
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
    _ <- LAsync.async (sendRequest recievingPeerNodeId PubSub message) -- need to handle errors
    sendPubSubMessage peerList message

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
                let newList = checkNodeTimers currList currTime
                let newSortedList = toSortedList newList
                writeTVar currListTvar newSortedList)
    maintainWatchersHelper watcherMap topicIdList

-- will take the list of watchers for each topic and check their validity
checkNodeTimers :: [NodeTimer] -> UTCTime -> [NodeTimer]
checkNodeTimers [] _ = []
checkNodeTimers (currNodeTimer:nodeTimerList) currentTime =
    if timer currNodeTimer < currentTime
        then [] ++ checkNodeTimers nodeTimerList currentTime
        else currNodeTimer : nodeTimerList

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
            let notifyMessageByteString = serialise notifyMessage
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
                then sendPubSubMessage watcherNodeIdList notifyMessageByteString
                else do
                    messageMapTuple <- liftIO $ readTVarIO $ fromJust currTuple
                    let messageNodeIdList = snd messageMapTuple
                    let finalListOfWatchers =
                            watcherNodeIdList List.\\ messageNodeIdList
                    sendPubSubMessage
                        finalListOfWatchers
                        notifyMessageByteString

-- | used by pubsub to update the dynamic resource to peer mapping when it receives a notify message for a particular dynamic resource
updateDynamicResourceToPeerMap ::
       (HasP2PEnv m, HasLogging m)
    => ResourceId
    -> ResourceHandler
    -> NodeId
    -> m ()
updateDynamicResourceToPeerMap resID resHandler nodeID = do
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    liftIO $
        atomically
            (do transientResourceToPeerMap <-
                    readTVar transientResourceToPeerMapTVar
                let currentEntry = HM.lookup resID transientResourceToPeerMap
                if isNothing currentEntry
                    then do
                        newNodeTVar <- newTVar [nodeID]
                        let modifiedMap =
                                HM.insert
                                    resID
                                    (resHandler, newNodeTVar)
                                    transientResourceToPeerMap
                        writeTVar transientResourceToPeerMapTVar modifiedMap
                    else do
                        let nodeListTVar = snd $ fromJust currentEntry
                        currNodeList <- readTVar nodeListTVar
                        let newNodeList = currNodeList ++ [nodeID]
                        writeTVar nodeListTVar newNodeList)

-- | Called by each service to register its Topics. Creates entries in TopicMap
-- the TopicHandler passed takes a topicmessage and returns a topic
-- if topicmessage is returned it is sent to all watchers
registerTopic ::
       (HasP2PEnv m, HasLogging m) => (Topic, TopicHandler, Int) -> m ()
registerTopic topicTuple = do
    topicHandlerTVar <- getTopicHandlerMapP2PEnv
    notifierTVar <- getNotifiersTableP2PEnv
    let topic = extractFirst topicTuple
    let topicHandler = extractSecond topicTuple
    let minimumNotifiersForTopic = extractThird topicTuple
    liftIO $
        atomically
            (do topicHandlerMap <- readTVar topicHandlerTVar
                let updatedMap = HM.insert topic topicHandler topicHandlerMap
                writeTVar topicHandlerTVar updatedMap)
    liftIO $
        atomically
            (do notifierTable <- readTVar notifierTVar
                let emptyNotifierList = toSortedList []
                emptyNotifierListTVar <- newTVar emptyNotifierList
                let updatedNotifierHashMap =
                        HM.insert
                            topic
                            (emptyNotifierListTVar, minimumNotifiersForTopic)
                            notifierTable
                writeTVar notifierTVar updatedNotifierHashMap)

-- | spawns a thread with the parameter TVar TopicMap and checks expired notifiers and possible resubscribe to the peer based on rep. also checks if min number if satisfied and add more peers accordingly by calling send options which takes peer from kad
maintainNotifiers :: (HasP2PEnv m, HasLogging m) => m ()
maintainNotifiers = do
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let topicIds = HM.keys notifierMap
    _ <- LAsync.async (maintainNotifiersHelper notifierMap topicIds)
    return ()

maintainNotifiersHelper ::
       (HasP2PEnv m, HasLogging m) => NotifiersTable -> [Topic] -> m ()
maintainNotifiersHelper _ [] = return ()
maintainNotifiersHelper notifierMap (currTopic:topicList) = do
    let currNotifierListTvar = HM.lookup currTopic notifierMap
    -- not checking for Nothing since the topic list is a list of keys for the map
    currTime <- liftIO getCurrentTime
    expiredNotifs <-
        liftIO $
        atomically
            (do currSortedList <- readTVar $ fst $ fromJust currNotifierListTvar
                let currNotiferList = fromSortedList currSortedList
                let nonExpiredNotifiers =
                        checkNodeTimers currNotiferList currTime
                let expiredNotifiers =
                        currNotiferList List.\\ nonExpiredNotifiers
                writeTVar
                    (fst $ fromJust currNotifierListTvar)
                    (toSortedList nonExpiredNotifiers) -- removing the expired notifiers
                return expiredNotifiers)
    resubscribeToExpiredPeers currTopic expiredNotifs -- there needs to a filtering of expired notifiers based on PeerReputation.
    maintainNotifiersHelper notifierMap topicList

resubscribeToExpiredPeers ::
       (HasP2PEnv m, HasLogging m) => Topic -> [Notifier] -> m ()
resubscribeToExpiredPeers _ [] = return ()
resubscribeToExpiredPeers mTopic (peer:peerList) = do
    _ <- LAsync.async (sendSubscribeToPeer mTopic peer)
    resubscribeToExpiredPeers mTopic peerList

sendSubscribeToPeer :: (HasP2PEnv m, HasLogging m) => Topic -> NodeTimer -> m ()
sendSubscribeToPeer mTopic currentNode = do
    let subMessage = Subscribe {topicId = mTopic, messageTimer = 30}
    let serializedSubMessage = serialise subMessage
    let notifierNodeId = timerNodeId currentNode
    currTime <- liftIO getCurrentTime
    response <- sendRequest notifierNodeId PubSub serializedSubMessage -- not exactly RPC, needs to be changed
    let responseMessage = deserialise response :: MessageTypePubSub
    case responseMessage of
        Response mresponseCode mTimer -- the notifier returns the actual time of the subscription
         -> do
            notifierTableTVar <- getNotifiersTableP2PEnv
            notifierMap <- liftIO $ readTVarIO notifierTableTVar
            let currNotifierListTvar = HM.lookup mTopic notifierMap
            case mresponseCode of
                Ok ->
                    liftIO $
                    atomically
                        (do currSortedList <-
                                readTVar $ fst $ fromJust currNotifierListTvar
                            let currNotiferList = fromSortedList currSortedList
                            let timeDiff = fromInteger mTimer :: NominalDiffTime
                            let subscriptionTime = addUTCTime timeDiff currTime
                            let newNotifier =
                                    NodeTimer
                                        { timerNodeId = notifierNodeId
                                        , timer = subscriptionTime
                                        }
                            let updatedList = currNotiferList ++ [newNotifier]
                            let updatedSortedList = toSortedList updatedList
                            writeTVar
                                (fst $ fromJust currNotifierListTvar)
                                updatedSortedList)
                Error -> return ()
        _ -> return () -- need a proper error message
-- maintainMinimumCountNotifier :: (HasP2PEnv m, HasLogging m) =>
