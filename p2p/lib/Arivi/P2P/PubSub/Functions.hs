{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, GADTs #-}

module Arivi.P2P.PubSub.Functions
    ( publishTopic
    , maintainWatchers
    , updateDynamicResourceToPeerMap -- remove form exported functions later
    , notifyTopic
    -- , registerTopic
    -- , maintainNotifiers
    , maintainMinimumCountNotifier -- exporting right now to not have warnings should be removed later
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        NodeId)
import           Arivi.P2P.MessageHandler.NodeEndpoint (issueRequest)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.P2P.PubSub.Types                hiding(Topic, topicId)
import qualified Arivi.P2P.PubSub.Types                as PTypes
import           Arivi.P2P.RPC.Types                   (ResourceHandler,
                                                        ResourceId)
                                                        
import           Arivi.Utils.Logging
import           Data.ByteString.Char8                 as Char8 (ByteString)
import           Codec.Serialise
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.STM

import qualified Arivi.P2P.Kademlia.Kbucket            as Kademlia (getKClosestPeersByNodeid,
                                                                    getKRandomPeers)
import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import           Arivi.P2P.RPC.Functions               (addPeerFromKademlia)
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (when)
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import           Data.Either.Unwrap
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
publishTopic :: (HasP2PEnv m, HasLogging m, Topic t, Serialise msg) => PubSubPublish t msg -> m ()
publishTopic payload@(PubSubPublish topic _ ) = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currWatcherListTvarMaybe = HM.lookup (topicId topic) watcherMap
    currWatcherSortedList <-
        case currWatcherListTvarMaybe of
            Nothing               -> return $ toSortedList []
            Just sortedWtListTVar -> liftIO $ readTVarIO sortedWtListTVar
    let currNotifierListTvarMaybe = HM.lookup (topicId topic) notifierMap
    currNotifierSortedList <-
        case currNotifierListTvarMaybe of
            Nothing -> return $ toSortedList []
            Just sortedNotListTVar ->
                liftIO $ readTVarIO $ fst sortedNotListTVar
    let currWatcherList = fromSortedList currWatcherSortedList
    let currNotifierList = fromSortedList currNotifierSortedList
    let combinedList = currWatcherList `List.union` currNotifierList
    let nodeIdList = List.map timerNodeId combinedList
    -- let message =
            -- Publish {topicId = messageTopic, topicMessage = publishMessage}
    -- let serializedMessage = serialise message
    sendPubSubPublish nodeIdList payload

sendPubSubPublish ::
       (HasP2PEnv m, HasLogging m, Topic t, Serialise msg) => [NodeId] -> PubSubPublish t msg -> m ()
sendPubSubPublish [] _ = return ()
sendPubSubPublish (recievingPeerNodeId:peerList) message = do
    -- _ <- LAsync.async (runExceptT $ issueRequest recievingPeerNodeId (PubSubRequest message)) -- need to handle errors
    sendPubSubPublish peerList message

sendPubSubNotify ::
       (HasP2PEnv m, HasLogging m, Topic t, Serialise msg) => [NodeId] -> PubSubNotify t msg -> m ()
sendPubSubNotify [] _ = return ()
sendPubSubNotify (recievingPeerNodeId:peerList) message = do
    -- _ <- LAsync.async (runExceptT $ issueRequest recievingPeerNodeId (PubSubRequest message)) -- need to handle errors
    sendPubSubNotify peerList message


maintainWatchers :: (HasP2PEnv m, HasLogging m) => m ()
maintainWatchers = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let topicIds = HM.keys watcherMap
    _ <- LAsync.async (maintainWatchersHelper watcherMap topicIds)
    return ()

maintainWatchersHelper ::
       (HasP2PEnv m, HasLogging m) => WatchersTable -> [PTypes.Topic] -> m ()
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

-- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers.
-- Only sent to Subscribers minus nodes in MessageHashMap
notifyTopic :: (HasP2PEnv m, HasLogging m, Topic t, Serialise msg) => PubSubNotify t msg -> m ()
notifyTopic payload@(PubSubNotify topic _ ) = do
    watcherTableTVar <- getWatcherTableP2PEnv
    watcherMap <- liftIO $ readTVarIO watcherTableTVar
    let listOfWatchersForTopic = HM.lookup (topicId topic) watcherMap
    case listOfWatchersForTopic of
        Nothing -> return () -- TODO :: what to do if no watchers are present for a given topic
        Just currWListTVar -> do
            -- let notifyMessage =
            --         Notify {topicId = mTopic, topicMessage = mTopicMessage}
            -- let notifyMessageByteString = serialise notifyMessage
            currWatcherSortedList <- liftIO $ readTVarIO currWListTVar
            let watcherList = fromSortedList currWatcherSortedList
            let watcherNodeIdList = List.map timerNodeId watcherList
            messageHashMapTvar <- getMessageHashMapP2PEnv
            messageHashMap <- liftIO $ readTVarIO messageHashMapTvar
            -- looking up random stuff just to make code compile for now
            let currTuple = HM.lookup "msg" messageHashMap
                    -- need to add an exception for failed sending of pubsub message
            case currTuple
                             -- there are no nodes for this message in the MessageHashMap so send to all the watchers
                  of
                Nothing ->
                    sendPubSubNotify watcherNodeIdList payload
                Just valueTuple -> do
                    messageMapTuple <- liftIO $ readTVarIO valueTuple
                    let messageNodeIdList = snd messageMapTuple
                    let finalListOfWatchers =
                            watcherNodeIdList List.\\ messageNodeIdList -- List.\\ does set difference
                    sendPubSubNotify
                        finalListOfWatchers
                        payload

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

-- | Called by each service to register its Topics. Creates entries in TopicMap
-- the TopicHandler passed takes a topicmessage and returns a topic
-- if topicmessage is returned it is sent to all watchers
-- registerTopic ::
--        (HasP2PEnv m, HasLogging m) => (Topic , TopicHandler, Int) -> m ()
-- registerTopic topicTuple = do
--     topicHandlerTVar <- getTopicHandlerMapP2PEnv
--     notifierTVar <- getNotifiersTableP2PEnv
--     let topic = extractFirst topicTuple
--     let topicHandler = extractSecond topicTuple
--     let minimumNotifiersForTopic = extractThird topicTuple
--     liftIO $
--         atomically
--             (do topicHandlerMap <- readTVar topicHandlerTVar
--                 let updatedMap = HM.insert topic topicHandler topicHandlerMap
--                 writeTVar topicHandlerTVar updatedMap)
--     liftIO $
--         atomically
--             (do notifierTable <- readTVar notifierTVar
--                 let emptyNotifierList = toSortedList []
--                 emptyNotifierListTVar <- newTVar emptyNotifierList
--                 let updatedNotifierHashMap =
--                         HM.insert
--                             topic
--                             (emptyNotifierListTVar, minimumNotifiersForTopic)
--                             notifierTable
--                 writeTVar notifierTVar updatedNotifierHashMap)

-- | spawns a thread with the parameter TVar TopicMap and checks expired notifiers and possible resubscribe to the peer based on rep. also checks if min number if satisfied and add more peers accordingly by calling send options which takes peer from kad
-- maintainNotifiers :: (HasP2PEnv m, HasLogging m) => m ()
-- maintainNotifiers = do
--     notifierTableTVar <- getNotifiersTableP2PEnv
--     notifierMap <- liftIO $ readTVarIO notifierTableTVar
--     let topicIds = HM.keys notifierMap
--     _ <- LAsync.async (maintainNotifiersHelper notifierMap topicIds)
--     return ()

maintainNotifiersHelper :: 
       (HasP2PEnv m, HasLogging m, Topic t) => NotifiersTable -> [t] -> m ()
maintainNotifiersHelper _ [] = return ()
maintainNotifiersHelper notifierMap (currTopic:topicList) = do
    let currNotifierListTvar = HM.lookup (topicId currTopic) notifierMap
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

subscribeToMultiplePeers ::
       (HasP2PEnv m, HasLogging m, Topic t) => t -> [NodeId] -> m ()
subscribeToMultiplePeers _ [] = return ()
subscribeToMultiplePeers mTopic (peer:peerList) = do
    _ <- LAsync.async (sendSubscribeToPeer mTopic peer)
    subscribeToMultiplePeers mTopic peerList

sendSubscribeToPeer :: (HasP2PEnv m, HasLogging m, Topic t) => t -> NodeId -> m ()
sendSubscribeToPeer mTopic notifierNodeId = do
    let subMessage = Subscribe mTopic 30
    -- let serializedSubMessage = serialise subMessage
    currTime <- liftIO getCurrentTime
    response <- runExceptT $ issueRequest notifierNodeId (PubSubRequest subMessage) -- not exactly RPC, needs to be changed
    -- let deserialiseCheck = deserialiseOrFail response
    case response of
        Left _ -> return () -- TODO:: reduce reputation if subscribe fails and handle the failure
        Right (PubSubResponse (SubscribeResponse (topic::PTypes.Topic) mresponseCode mTimer)) -> do
            notifierTableTVar <- getNotifiersTableP2PEnv
            liftIO $
                atomically
                    (do
                        notifierMap <- readTVar notifierTableTVar
                        let currNotifierListTvar =
                                HM.lookup topic notifierMap
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
                                                    topic
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
                            Error -> return ()
                     )

-- might have to write a wrapper above this
-- need to take nodeID from env
maintainMinimumCountNotifier :: (HasP2PEnv m, HasLogging m, Topic t) => t -> m ()
maintainMinimumCountNotifier mTopic = do
    -- currNodeId <- getSelfNodeId
    notifierTableTVar <- getNotifiersTableP2PEnv
    notifierMap <- liftIO $ readTVarIO notifierTableTVar
    let currNotifierListTuple = HM.lookup (topicId mTopic) notifierMap
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
                    -- getNodeId somehow
                    Kademlia.getKClosestPeersByNodeid "currNodeId" 3
                peersClose <-
                    case res1 of
                        Left (_ :: Exception.SomeException) ->
                            Kademlia.getKRandomPeers 3
                        Right peers -> return (fromRight peers)
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                subscribeToMultiplePeers mTopic nodeIds
