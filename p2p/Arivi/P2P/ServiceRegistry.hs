--{-# LANGUAGE DeriveGeneric         #-}

-- |
-- Module      :  Arivi.P2P.ServiceRegistry
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- ServiceRegistry is part of Arivi P2P layer, It keeps track of ServiceContext
-- and ConnectionCommand
module Arivi.P2P.ServiceRegistry
(
    --   ConnectionCommand(..)
    -- , ContextId
    --   ServiceContext
    -- , genUniqueSessionId
    -- , registerTopic
) where

import           Arivi.Kademlia.Query
import           Arivi.Network.Connection     (Connection)
import           Arivi.Network.Types          (ConnectionId, NodeId,
                                               TransportType (..))
import           Arivi.P2P.MessageHandler
import           Arivi.P2P.Types
import           Codec.Serialise              (deserialise, serialise)
import           Control.Concurrent
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception            (SomeException, try)
import           Control.Monad
import           Data.ByteString.Char8        as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy         as ByteStringLazy (ByteString,
                                                                 toStrict)
import           Data.HashTable.IO
import qualified Data.List                    as List
import qualified Data.Map                     as Map
import           Data.Maybe                   (Maybe, fromJust, fromMaybe)
import qualified Data.Set                     as Set
import           Data.Time.Clock
import           Data.UUID                    (UUID)
import           Data.UUID.V1                 (nextUUID)
import           Network                      (PortID (..), PortNumber (..),
                                               connectTo)
import           System.IO



-- fill hash with empty lists ServicePeerList
makeP2Pinstance :: TVar AriviP2PInstance -> TVar WatchersMap -> TVar SubscriptionMap -> TVar PeerToTopicMap
                   -> TVar TopicContext -> NodeId -> String -> Int -> Float -> Int -> IO ()
makeP2Pinstance ariviP2PInstanceTvar watchersMapTvar subscriptionMapTvar peerToTopicMapTvar
                topicContextTvar nodeId ip port outboundPeerQuota maxConnectionAllowed = do

    atomically( writeTVar ariviP2PInstanceTvar ( AriviP2PInstance nodeId ip port
                                                 outboundPeerQuota maxConnectionAllowed) )

    let watchersMap = Map.fromList     [ (LatestBlock,     [])]
    let subscriptionMap = Map.fromList [ (LatestBlock,     [])]

    atomically( writeTVar watchersMapTvar watchersMap )
    atomically( writeTVar subscriptionMapTvar subscriptionMap )

    forkIO ( handleIncomingConnections ariviP2PInstanceTvar watchersMapTvar subscriptionMapTvar
                                       topicContextTvar outboundPeerQuota maxConnectionAllowed )


    -- thread for asking kademlia for peer and ask for topics available
    -- and modify GLOBAL MATRIX which maintains the ranking of peers
    -- which provide more topics.

    return ()

-- API provided to service layer to send Serivce with all the topics in it to be registered at p2p layer
registerService :: TVar AriviP2PInstance -> TVar SubscriptionMap -> TVar WatchersMap-> TVar PeerToTopicMap
                   -> TVar TopicContext -> ServiceCode -> [TopicCode] -> MinPeerCountPerTopic ->
                   TransportType -> NodeType -> IO ()
registerService ariviP2PInstanceTvar subscriptionMapTvar watchersMapTvar peerToTopicMapTvar 
                topicContextTvar serviceCode topicCodeList minPeerCountPerTopic transport peerType = do
                
                -- map topictoservice and servicetotopic
                
                -- register each topic provided in topicCodelist 
                _ <- mapM (registerTopic ariviP2PInstanceTvar subscriptionMapTvar watchersMapTvar peerToTopicMapTvar 
                            topicContextTvar minPeerCountPerTopic transport peerType) topicCodeList
                return ()

insertIntoTopicToServiceMap :: TVar TopicToServiceMap -> [TopicCode] -> ServiceCode -> IO ()
insertIntoTopicToServiceMap topicToServiceMapTvar topicCodeList serviceCode = 
    when (topicCodeList /= []) (do
        atomically( modifyTVar topicToServiceMapTvar (Map.insert  (head topicCodeList) serviceCode) )
        insertIntoTopicToServiceMap topicToServiceMapTvar (tail topicCodeList) serviceCode )

-- ======== Private functions =========

-- To newly register provided topic into TopicContext, SubscriptionMap and WatcherMap
registerTopic :: TVar AriviP2PInstance -> TVar SubscriptionMap -> TVar WatchersMap-> TVar PeerToTopicMap
                 -> TVar TopicContext  -> MinPeerCountPerTopic -> TransportType -> NodeType -> TopicCode -> IO ()
registerTopic ariviP2PInstanceTvar subscriptionMapTvar watchersMapTvar peerToTopicMapTvar 
              topicContextTvar minPeerCountPerTopic transport peerType topicCode = do

    atomically( modifyTVar topicContextTvar (registerTopicInTopicCntxtTvar topicCode 
                                            minPeerCountPerTopic peerType transport))
    atomically( modifyTVar subscriptionMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))
    atomically( modifyTVar watchersMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))

    ariviP2PInstance <- atomically( readTVar ariviP2PInstanceTvar )

    let outboundPeerQuota' = outboundPeerQuota ariviP2PInstance
    let maxConnectionAllowed' = maxConnectionAllowed ariviP2PInstance

    topicContext <- atomically( readTVar topicContextTvar )
    
    _ <- forkIO (addSubscriberThread topicCode topicContext subscriptionMapTvar 
                           watchersMapTvar peerToTopicMapTvar minPeerCountPerTopic maxConnectionAllowed')

    return ()


-- To register a new Topic in the given topicContext with required context details 
registerTopicInTopicCntxtTvar :: TopicCode -> MinPeerCountPerTopic -> NodeType -> 
    TransportType-> TopicContext -> TopicContext
registerTopicInTopicCntxtTvar topicCode minPeerCountPerTopic peerType transport topCntxt = 
    let context = (minPeerCountPerTopic, peerType, transport)
    in  Map.insert topicCode context topCntxt

-- To register/initalize the new topic if it does not already existing in Subscription Map and Watchers Map 
registerTopicSubMapOrWatchMapTvar topicCode subscriptionSubOrWatchMap =
    let hasTopicCode = Map.lookup topicCode subscriptionSubOrWatchMap
    in  case hasTopicCode of Nothing ->  Map.insert topicCode [] subscriptionSubOrWatchMap
                             _ -> subscriptionSubOrWatchMap



-- Check if we have any incoming peers asking for subscription later add them if raito is not violated
handleIncomingConnections :: TVar AriviP2PInstance -> TVar WatchersMap -> TVar SubscriptionMap -> 
        TVar TopicContext -> Float -> Int -> IO ()
handleIncomingConnections ariviP2PInstanceTvar watchersMapTvar subscriptionMapTvar
     topicContextTvar outboundPeerQuota maxConnectionAllowed = forever $ do

    -- Network layer Blocking Call *API* which gives ConnectionId of any incoming peers
    let connId = getNewConnection

    forkIO ( processIncomingFromConnection connId )



processIncomingFromConnection :: ConnectionId -> IO ()
processIncomingFromConnection connId = forever $ do
    -- check (if connection is broken or if kill message is recived ): kill this thread 
    
    -- Network Layer *API* which read message provided connectionId
    -- msg = readMessage connId

    -- check what kind of message is recieved that is:
    -- PubSub -> subscribe, notify, publish, response
    -- RPC    -> Options, GET, Return

    -- case message of 
    --     Subscribe -> processIncomingSubscribe
    --     Notify    -> processIncomingNotify
    --     Publish   -> processIncomingPublish
    --     Options   -> processIncomingOptions
    --     Get       -> processIncomingGet
    --     Return    -> processIncomingReturn
    --     Response  -> processIncomingResponse
    --     _         -> ErrorHandlerForP2PMessage

    processIncomingFromConnection connId

-- functions to process each type of message provided by p2p layer
-- processIncomingSubscribe
-- processIncomingNotify
-- processIncomingPublish
-- processIncomingResponse
-- processIncomingOptions
-- processIncomingGet
-- processIncomingReturn
-- ErrorHandlerForP2PMessage


intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

willPeerAdditionViolateRaito :: TVar WatchersMap -> TVar SubscriptionMap -> TVar TopicContext ->
    TopicCode -> Float -> Int -> IO Bool
willPeerAdditionViolateRaito watchersMapTvar subscriptionMapTvar topicContextTvar
                             topicCode outboundPeerQuota maxConnectionAllowed = do

    watchersMap     <- atomically( readTVar watchersMapTvar )
    subscriptionMap <- atomically( readTVar subscriptionMapTvar )

    let uniqueLen       = length . Set.fromList . List.intercalate [] . Map.elems
    let subscriptionLen = uniqueLen subscriptionMap
    let watchersLen     = uniqueLen watchersMap

    -- Addition of 1 is to check that will ratio be violated after adding the peer
    let totalCount        =  subscriptionLen + watchersLen + 1 
    let subscribeToTotalRatio = intToFloat subscriptionLen / intToFloat totalCount 

    return ((totalCount < maxConnectionAllowed) && (subscribeToTotalRatio >= outboundPeerQuota))

getNodeIdFromContext :: (NodeId, IP, Port, ExpiryTime) -> NodeId
getNodeIdFromContext (nodeId,_,_,_) = nodeId 

-- Finds out if topic needs peer based on minimun peer count taking TopicCode as input
neededPeerCount :: TopicCode -> TopicContext -> SubscriptionMap -> WatchersMap -> Int
neededPeerCount topicCode topicContextMap subscriptionMap watchersMap = 
    let 
    -- In case lookup gives a nothing fromMaybe will take care of it
    topicContext   = Map.lookup topicCode topicContextMap
    subpeerlist    = fromMaybe [] (Map.lookup topicCode subscriptionMap)
    notifypeerlist = fromMaybe [] (Map.lookup topicCode watchersMap)

    peerlist = map getNodeIdFromContext subpeerlist ++  map  getNodeIdFromContext notifypeerlist
    totalconnectioncount = length $ Set.fromList  peerlist

    minimumPeerConnection = case topicContext of Nothing -> 0
                                                 Just (peerCount, _, _) -> peerCount
    in  minimumPeerConnection - totalconnectioncount 


-- It checks minimum PeerConnection requirement and sends subscribe messages to Peer
addSubscriberThread :: TopicCode -> TopicContext -> TVar SubscriptionMap -> TVar WatchersMap 
                       -> TVar PeerToTopicMap -> MinPeerCountPerTopic -> Int -> IO ()
addSubscriberThread topicCode topicContext subscriptionMapTvar watchersMapTvar 
                    peerToTopicMapTvar minPeerCount maxConnectionAllowed = forever $ do

    -- check if topic was de-registered from TopicContext then kill this thread

    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    watchersMap <- atomically( readTVar watchersMapTvar )

    let uniqueLen = length . Set.fromList . List.intercalate [] . Map.elems
    let subscriptionLen = uniqueLen subscriptionMap
    let totalCount = subscriptionLen + uniqueLen watchersMap

    let numOfPeerNeeded = neededPeerCount topicCode topicContext subscriptionMap watchersMap

    if totalCount < maxConnectionAllowed && numOfPeerNeeded > 0 then
        do
            _ <- sendSubscriptionToNPeers topicCode peerToTopicMapTvar numOfPeerNeeded

            -- wait for certain timeout 30 seconds before checking or sending subscribe message to a another peer
            threadDelay (30*1000000)
            return ()
    else 
        do
            -- wait for certain timeout 100 seconds before checking or sending subscribe message to a another peer
            threadDelay (100*1000000)
            return ()

sendSubscriptionToNPeers :: TopicCode -> TVar PeerToTopicMap -> Int -> IO ()
sendSubscriptionToNPeers topicCode peerToTopicMapTvar num = do
    -- get peer from topicToPeerMap
    peer <- getPeer topicCode peerToTopicMapTvar

    -- asking network layer gives connectionId first by checking if connection already exists and if not by forming a new one 
    let connId = openConnection (peerNodeId peer) (peerIp peer) (peerPort peer) (peerTransportType peer)

    -- sending a subscribe message to kademila provided nodeId for given topic 
    subscribeMessage connId topicCode
    when (num > 0) (sendSubscriptionToNPeers topicCode peerToTopicMapTvar (num-1) )

-- Get peer from PeerToTopicMap for a particular topicCode of no peer found
-- then add a new peer to the
getPeer :: TopicCode -> TVar PeerToTopicMap -> IO Peer
getPeer topicCode peerToTopicMapTvar = do

    peer <- atomically ( readPeerandModify  topicCode peerToTopicMapTvar )

    case peer of
        Nothing ->
            do
                _ <- sendOptionsMessageToGetPeer

                -- wait for certain time period t0
                -- wait (t0)

                getPeer topicCode peerToTopicMapTvar

        Just peer -> return peer


-- Reads peerlist from PeerToTopicMap using TopicCode and takes first peer in the list and returns the peer
-- also putting the peer at the end of the peerlist and updating PeerToTopicMap
-- In case topicCode is not in Map or if peerlist is empty then it will return Nothing
readPeerandModify :: TopicCode ->TVar PeerToTopicMap -> STM (Maybe Peer)
readPeerandModify  topicCode peerToTopicMapTvar = do
    peerToTopicMap <- readTVar peerToTopicMapTvar
    let peerlist =  Map.lookup topicCode  peerToTopicMap

    case peerlist of
         Nothing -> return Nothing
         _ -> do
            let plist = fromJust peerlist
            case plist of
                [] -> return Nothing
                _  -> do
                    let (firstPeer:remainingList) = plist
                    let peerToTopicMap = Map.insert topicCode (remainingList++[firstPeer]) peerToTopicMap
                    _ <- writeTVar peerToTopicMapTvar peerToTopicMap
                    return $ Just firstPeer


-- when no peer was found for a topicCode or resourceId
-- gets peer peers from kademlia and sends them options message
sendOptionsMessageToGetPeer :: IO ()
sendOptionsMessageToGetPeer = do

    -- kademila layer *API* -- get n peers from kademlia for now its 10
    let peerlist = kademilaGetPeer 10

        --temp tvar later shifted to env
    -- send options message
    sendOptionsMessage peerlist

subscribeMessage :: ConnectionId -> TopicCode -> IO ()
subscribeMessage connId topicNeeded =

       -- Network Layer API call
       sendMessage connId (ByteStringLazy.toStrict $ serialise topicNeeded)



-- ===========================================================================================
-- ======================       DUMMY functions          ======================================
-- ===========================================================================================

-- Dummy functions to mimic API provided by other layers
-- getPeer :: TopicCode -> Peer
-- getPeer topicCode = Peer{ peerNodeId = ( pack "892346384") , peerIp = "127.0.0.1", peerPort = 300, peerTransportType = TCP}

openConnection :: NodeId -> IP -> Port -> TransportType -> ConnectionId
openConnection nodeId ip port transportType = pack "892sadasd346384"

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()

getNewConnection :: ConnectionId
getNewConnection = pack "892sadasd346384"

kademilaGetPeer :: Int -> [Peer]
kademilaGetPeer count = [Peer { peerNodeId = pack "892346384" , peerIp = "127.0.0.1", peerPort = 300, peerTransportType = TCP},
                            Peer { peerNodeId = pack "892346384" , peerIp = "127.0.0.1", peerPort = 300, peerTransportType = TCP}]
--to be replaced with sendOptionMessage from messagehandler
sendOptionsMessage :: [Peer] -> IO ()
sendOptionsMessage [peer] = do
    let connId = openConnection (peerNodeId peer) (peerIp peer) (peerPort peer) (peerTransportType peer)
    -- Network Layer API call
    sendMessage connId (ByteStringLazy.toStrict $ serialise Options)
sendOptionsMessage (peer:peerlist) = do
    let connId = openConnection (peerNodeId peer) (peerIp peer) (peerPort peer) (peerTransportType peer)
    -- Network Layer API call
    sendMessage connId (ByteStringLazy.toStrict $ serialise Options)
