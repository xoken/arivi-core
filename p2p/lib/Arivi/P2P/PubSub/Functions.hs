{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.PubSub.Functions
    ( publishTopic
    , maintainWatchers
    , notifyTopic
    , registerTopic
    , maintainNotifiers
    , updateTransientResourceToPeerMap
    , maintainMinimumCountNotifier -- exporting right now to not have warnings should be removed later
    , pubsubHandler
    ) where

import           Arivi.P2P.Exception
import qualified Arivi.P2P.Kademlia.Kbucket            as Kademlia (getKClosestPeersByNodeid,
                                                                    getKRandomPeers)
import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import           Arivi.P2P.MessageHandler.Handler      (sendRequest)
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        NodeId, P2PPayload)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Functions               (addPeerFromKademlia)
import           Arivi.P2P.RPC.Types                   (ResourceHandler,
                                                        ResourceId)
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialiseOrFail,
                                                        serialise)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (unless, when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (pack)
import qualified Data.ByteString.Lazy                  as Lazy (ByteString,
                                                                fromStrict)
import           Data.Either.Unwrap
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List                             as List
import           Data.Maybe
import           Data.SortedList                       as SortedList
import           Data.Time.Clock

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

-- | called by a service when some content created by it needs to be broad-casted to the network i.e. notifiers as well as subscribers
publishTopic :: (HasP2PEnv m, HasLogging m) => Topic -> TopicMessage -> m ()
publishTopic messageTopic publishMessage = do
    mNodeId <- getSelfNodeId
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currWatcherListTvarMaybe = HM.lookup messageTopic watcherMap
    currWatcherSortedList <-
        case currWatcherListTvarMaybe of
            Nothing               -> return $ toSortedList []
            Just sortedWtListTVar -> liftIO $ readTVarIO sortedWtListTVar
    let currNotifierListTvarMaybe = HM.lookup messageTopic notifierMap
    currNotifierSortedList <-
        case currNotifierListTvarMaybe of
            Nothing -> return $ toSortedList []
            Just sortedNotListTVar ->
                liftIO $ readTVarIO $ fst sortedNotListTVar
    let currWatcherList = fromSortedList currWatcherSortedList
    let currNotifierList = fromSortedList currNotifierSortedList
    let combinedList = currWatcherList `List.union` currNotifierList
    let nodeIdList = List.map timerNodeId combinedList
    let message =
            Publish
                { nodeId = mNodeId
                , topicId = messageTopic
                , topicMessage = publishMessage
                }
    let serializedMessage = serialise message
    sendMultiplePubSubMessage nodeIdList serializedMessage

-- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers.
-- Only sent to Subscribers minus nodes in MessageHashMap
notifyTopic :: (HasP2PEnv m, HasLogging m) => Topic -> TopicMessage -> m ()
notifyTopic mTopic mTopicMessage = do
    mNodeId <- getSelfNodeId
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let listOfWatchersForTopic = HM.lookup mTopic watcherMap
    case listOfWatchersForTopic of
        Nothing -> return () -- TODO :: what to do if no watchers are present for a given topic
        Just currWListTVar -> do
            let notifyMessage =
                    Notify
                        { nodeId = mNodeId
                        , topicId = mTopic
                        , topicMessage = mTopicMessage
                        }
            let notifyMessageByteString = serialise notifyMessage
            currWatcherSortedList <- liftIO $ readTVarIO currWListTVar
            let watcherList = fromSortedList currWatcherSortedList
            let watcherNodeIdList = List.map timerNodeId watcherList
            messageHashMapTvar <- getMessageHashMapP2PEnv
            messageHashMap <- liftIO $ readTVarIO messageHashMapTvar
            let currTuple = HM.lookup mTopicMessage messageHashMap
                    -- need to add an exception for failed sending of pubsub message
            case currTuple
                             -- there are no nodes for this message in the MessageHashMap so send to all the watchers
                  of
                Nothing ->
                    sendMultiplePubSubMessage
                        watcherNodeIdList
                        notifyMessageByteString
                Just valueTuple -> do
                    messageNodeIdList <- liftIO $ readTVarIO valueTuple
                    let finalListOfWatchers =
                            watcherNodeIdList List.\\ messageNodeIdList -- List.\\ does set difference
                    sendMultiplePubSubMessage
                        finalListOfWatchers
                        notifyMessageByteString

subscribeToMultiplePeers ::
       (HasP2PEnv m, HasLogging m) => Topic -> [NodeId] -> m ()
subscribeToMultiplePeers _ [] = return ()
subscribeToMultiplePeers mTopic (peer:peerList) = do
    _ <- LAsync.async (sendSubscribeToPeer mTopic peer)
    subscribeToMultiplePeers mTopic peerList

sendSubscribeToPeer :: (HasP2PEnv m, HasLogging m) => Topic -> NodeId -> m ()
sendSubscribeToPeer mTopic notifierNodeId = do
    mNodeId <- getSelfNodeId
    let subMessage =
            Subscribe {nodeId = mNodeId, topicId = mTopic, messageTimer = 30}
    let serializedSubMessage = serialise subMessage
    currTime <- liftIO getCurrentTime
    response <- sendRequest notifierNodeId PubSub serializedSubMessage
    let deserialiseCheck = deserialiseOrFail response
    case deserialiseCheck of
        Left _ -> return () -- TODO:: reduce reputation if subscribe fails and handle the failure
        Right (responseMessage :: MessageTypePubSub) -> do
            notifierTableTVar <- getNotifiersTableP2PEnv
            liftIO $
                atomically
                    (case responseMessage of
                         Response mresponseCode mTimer -- the notifier returns the actual time of the subscription
                          -> do
                             notifierMap <- readTVar notifierTableTVar
                             let currNotifierListTvar =
                                     HM.lookup mTopic notifierMap
                             case mresponseCode of
                                 Ok -> do
                                     let timeDiff =
                                             fromInteger mTimer :: NominalDiffTime
                                     let subscriptionTime =
                                             addUTCTime timeDiff currTime
                                     let newNotifier =
                                             NodeTimer
                                                 { timerNodeId = notifierNodeId
                                                 , timer = subscriptionTime
                                                 }
                                     case currNotifierListTvar of
                                         Nothing -> do
                                             let newNotif =
                                                     toSortedList [newNotifier]
                                             newNotifTvar <- newTVar newNotif
                                                -- TODO:: read the minimum number of notifs from config instead of hardcoding
                                             let updatedMap =
                                                     HM.insert
                                                         mTopic
                                                         (newNotifTvar, 5)
                                                         notifierMap
                                             writeTVar
                                                 notifierTableTVar
                                                 updatedMap
                                         Just entry -> do
                                             currSortedList <-
                                                 readTVar $ fst entry
                                             let currNotiferList =
                                                     fromSortedList
                                                         currSortedList
                                             let updatedList =
                                                     currNotiferList ++
                                                     [newNotifier]
                                             let updatedSortedList =
                                                     toSortedList updatedList
                                             writeTVar
                                                 (fst entry)
                                                 updatedSortedList
                                 _ -> return ()
                         _ -> throw PubSubInvalidResponseException)

sendMultiplePubSubMessage ::
       (HasP2PEnv m, HasLogging m) => [NodeId] -> Lazy.ByteString -> m ()
sendMultiplePubSubMessage [] _ = return ()
sendMultiplePubSubMessage (recievingPeerNodeId:peerList) message = do
    _ <- LAsync.async (sendRequest recievingPeerNodeId PubSub message) -- need to handle errors
    sendMultiplePubSubMessage peerList message

-- | used by pubsub to update the dynamic resource to peer mapping when it receives a notify message for a particular dynamic resource
updateTransientResourceToPeerMap ::
       (HasP2PEnv m, HasLogging m)
    => ResourceId
    -> ResourceHandler
    -> NodeId
    -> m ()
updateTransientResourceToPeerMap resID resHandler nodeID = do
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    liftIO $
        atomically
            (do transientResourceToPeerMap <-
                    readTVar transientResourceToPeerMapTVar
                let currentEntry = HM.lookup resID transientResourceToPeerMap
                case currentEntry of
                    Nothing -> do
                        newNodeTVar <- newTVar [nodeID]
                        let modifiedMap =
                                HM.insert
                                    resID
                                    (resHandler, newNodeTVar)
                                    transientResourceToPeerMap
                        writeTVar transientResourceToPeerMapTVar modifiedMap
                    Just entryValue -> do
                        let nodeListTVar = snd entryValue
                        currNodeList <- readTVar nodeListTVar
                        let newNodeList = currNodeList ++ [nodeID]
                        writeTVar nodeListTVar newNodeList)

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
    case currListTvarMaybe of
        Nothing -> return () -- If topic does not exist return
        Just currListTvar -> do
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
    case currNotifierListTvar of
        Nothing -> return ()
        Just mapValue -> do
            currTime <- liftIO getCurrentTime
            expiredNotifs <-
                liftIO $
                atomically
                    (do currSortedList <- readTVar $ fst mapValue
                        let currNotiferList = fromSortedList currSortedList
                        let nonExpiredNotifiers =
                                checkNodeTimers currNotiferList currTime
                        let expiredNotifiers =
                                currNotiferList List.\\ nonExpiredNotifiers
                        writeTVar
                            (fst $ fromJust currNotifierListTvar)
                            (toSortedList nonExpiredNotifiers) -- removing the expired notifiers
                        return expiredNotifiers)
            let expiredNodeIds = List.map timerNodeId expiredNotifs
            subscribeToMultiplePeers currTopic expiredNodeIds -- there needs to a filtering of expired notifiers based on PeerReputation.
            maintainNotifiersHelper notifierMap topicList

-- TODO :: call this function in the correct context
maintainMinimumCountNotifier :: (HasP2PEnv m, HasLogging m) => Topic -> m ()
maintainMinimumCountNotifier mTopic = do
    currNodeId <- getSelfNodeId
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currNotifierListTuple = HM.lookup mTopic notifierMap
    case currNotifierListTuple of
        Nothing -> return ()
        Just entry -> do
            currSortedList <- liftIO $ readTVarIO $ fst entry
            let minNumberOfNotifiers = snd $ fromJust currNotifierListTuple
            let currNotiferList = fromSortedList currSortedList
            Control.Monad.when
                (length currNotiferList - minNumberOfNotifiers < 0) $ do
                peerRandom <- Kademlia.getKRandomPeers 2
                res1 <-
                    Exception.try $
                    Kademlia.getKClosestPeersByNodeid currNodeId 3
                peersClose <-
                    case res1 of
                        Left (_ :: Exception.SomeException) ->
                            Kademlia.getKRandomPeers 3
                        Right peers -> return (fromRight peers)
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                subscribeToMultiplePeers mTopic nodeIds

-- | handler function for incoming pubsub messages
-- TODO :: Incoming response messages are not handled currently since an error message will reduce the Reputation etc.
pubsubHandler :: (HasP2PEnv m, HasLogging m) => P2PPayload -> m P2PPayload
pubsubHandler incomingRequest = do
    messageMapTVar <- getMessageHashMapP2PEnv
    notifierTableTVar <- getNotifiersTableP2PEnv
    topicHandlerTVar <- getTopicHandlerMapP2PEnv
    watcherMapTVar <- getWatcherTableP2PEnv
    let deserialiseCheck = deserialiseOrFail incomingRequest
    case deserialiseCheck of
        Left _ -- the to = NodeId should be entered by P2P/Node end point
         -> do
            let errorMessage =
                    Response
                        { responseCode = DeserialiseError
                        , messageTimer = 0 -- Error Message, setting timer as 0
                        }
            return $ serialise errorMessage
        Right (incomingMessage :: MessageTypePubSub) ->
            case incomingMessage of
                Notify recvNodeId mTopicId mTopicMessage ->
                    notifyMessageHandler
                        recvNodeId
                        mTopicId
                        mTopicMessage
                        messageMapTVar
                        notifierTableTVar
                        topicHandlerTVar
                Publish recvNodeId mTopicId mTopicMessage ->
                    publishMessageHandler
                        recvNodeId
                        mTopicId
                        mTopicMessage
                        messageMapTVar
                        notifierTableTVar
                        watcherMapTVar
                        topicHandlerTVar
                Subscribe recvNodeId mTopicId mMessageTimer ->
                    subscribeMessageHandler
                        recvNodeId
                        mTopicId
                        mMessageTimer
                        watcherMapTVar
                Response rCode _ -> responseMessageHandler rCode

notifyMessageHandler ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> Topic
    -> TopicMessage
    -> TVar MessageHashMap
    -> TVar NotifiersTable
    -> TVar TopicHandlerMap
    -> m P2PPayload
notifyMessageHandler recvNodeId mTopicId mTopicMessage messageMapTVar notifierTableTVar topicHandlerTVar = do
    notifierTable <- liftIO $ readTVarIO notifierTableTVar
    let entry = HM.lookup mTopicId notifierTable
    case entry
        -- check if the notify message came from a valid notifier
          of
        Nothing -> return $ Lazy.fromStrict $ pack "Topic does not exist"
        Just notifierListTuple -> do
            notifierSortedList <- liftIO $ readTVarIO $ fst notifierListTuple
            let listOfNodeIds = SortedList.map timerNodeId notifierSortedList
            if recvNodeId `elemOrd` listOfNodeIds
                then verifyIncomingMessage
                         recvNodeId
                         mTopicId
                         mTopicMessage
                         messageMapTVar
                         topicHandlerTVar
                else throw PubSubInvalidNotifierException

publishMessageHandler ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> Topic
    -> TopicMessage
    -> TVar MessageHashMap
    -> TVar NotifiersTable
    -> TVar WatchersTable
    -> TVar TopicHandlerMap
    -> m P2PPayload
publishMessageHandler recvNodeId mTopicId mTopicMessage messageMapTVar notifierTableTVar watcherMapTVar topicHandlerTVar = do
    notifierTable <- liftIO $ readTVarIO notifierTableTVar
    watcherTable <- liftIO $ readTVarIO watcherMapTVar
    let currWatcherListTvarMaybe = HM.lookup mTopicId watcherTable
    currWatcherSortedList <-
        case currWatcherListTvarMaybe of
            Nothing               -> return $ toSortedList []
            Just sortedWtListTVar -> liftIO $ readTVarIO sortedWtListTVar
    let currNotifierListTvarMaybe = HM.lookup mTopicId notifierTable
    currNotifierSortedList <-
        case currNotifierListTvarMaybe of
            Nothing -> return $ toSortedList []
            Just sortedNotifListTVar ->
                liftIO $ readTVarIO $ fst sortedNotifListTVar
    let currWList = fromSortedList currWatcherSortedList
    let currNotifList = fromSortedList currNotifierSortedList
    let combinedList = currWList `List.union` currNotifList
    let listOfNodeIds = List.map timerNodeId combinedList
    if recvNodeId `elem` listOfNodeIds
        then verifyIncomingMessage
                 recvNodeId
                 mTopicId
                 mTopicMessage
                 messageMapTVar
                 topicHandlerTVar
        else throw PubSubInvalidPublisherException

-- | used by notifyHandler and PublishHandler to verify the incoming message or add to message map if the message has already been received
verifyIncomingMessage ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> Topic
    -> TopicMessage
    -> TVar MessageHashMap
    -> TVar TopicHandlerMap
    -> m P2PPayload
verifyIncomingMessage recvNodeId mTopicId mTopicMessage messageMapTVar topicHandlerTVar = do
    returnMessage <-
        liftIO $
        atomically
            (do messageMap <- readTVar messageMapTVar
                let entryInMessageMap = HM.lookup mTopicMessage messageMap
                case entryInMessageMap of
                    Nothing -> do
                        newList <- newTVar [recvNodeId]
                        let newMap = HM.insert mTopicMessage newList messageMap
                        writeTVar messageMapTVar newMap
                        topicHandlerMap <- readTVar topicHandlerTVar
                        let entryTopicMap = HM.lookup mTopicId topicHandlerMap
                        case entryTopicMap of
                            Nothing -> throw PubSubTopicNotRegisteredException
                            Just topicHandler -> do
                                let verifiedMessage = topicHandler mTopicMessage
                            -- TODO :: check validity
                            -- TODO :: To add resource in transient resource map need the handler.
                                let flag1 = 1 :: Integer
                                return (flag1, verifiedMessage)
                -- if the entry exists then it is a duplicate notify
                    Just valueInMap -> do
                        nodeIdList <- readTVar valueInMap
                        let temp = recvNodeId `elem` nodeIdList
                        unless temp $ do
                            let updatedNodeIdList = nodeIdList ++ [recvNodeId]
                            writeTVar valueInMap updatedNodeIdList
                        let flag0 = 0 :: Integer
                        return (flag0, pack "Duplicate Message"))
    let checkReturn = fst returnMessage
    -- let messageAfterVerif = snd returnMessage
    if checkReturn == 1
        then do
            notifyTopic mTopicId (snd returnMessage)
            return $ serialise mTopicMessage
        else throw PubSubDuplicateMessageException

-- | Handler for incoming subscribe message, If the node is the first subscriber create a list for the topic and add the node
-- if the topic already exists then add the node to the list of watchers
-- TODO :: Set the timer specifically depending upon some metric
subscribeMessageHandler ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> Topic
    -> Integer
    -> TVar WatchersTable
    -> m P2PPayload
subscribeMessageHandler mNodeId mTopic mTime watcherTableTVar = do
    let responseMessage = Response {responseCode = Ok, messageTimer = mTime}
    let serializedMessage = serialise responseMessage
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let entry = HM.lookup mTopic watcherMap
    currTime <- liftIO getCurrentTime
    let timeDiff = fromInteger mTime :: NominalDiffTime
    let subscriptionTime = addUTCTime timeDiff currTime
    let watcher = NodeTimer {timerNodeId = mNodeId, timer = subscriptionTime}
    case entry of
        Nothing ->
            liftIO $
            atomically $ do
                let temp = toSortedList [watcher]
                watcherSortedList <- newTVar temp
                let newMap = HM.insert mTopic watcherSortedList watcherMap
                writeTVar watcherTableTVar newMap
        Just watcherTVar ->
            liftIO $
            atomically
                (do watcherSortedList <- readTVar watcherTVar
                    let watcherList = fromSortedList watcherSortedList
                    let newList = toSortedList $ watcherList ++ [watcher]
                    writeTVar watcherTVar newList)
    sendRequest mNodeId PubSub serializedMessage

-- | Dummy handler for the incoming response message needs to increment or decrement the Reputation
responseMessageHandler ::
       (HasP2PEnv m, HasLogging m) => ResponseCode -> m P2PPayload
responseMessageHandler rCode =
    case rCode of
        Ok -> do
            let resp = rCode
            let serializedMessage = serialise resp
            return serializedMessage
        _ -> do
            let errResp = rCode
            let serializedEMessage = serialise errResp
            return serializedEMessage
