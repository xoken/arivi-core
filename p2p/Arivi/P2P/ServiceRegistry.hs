
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

import           Arivi.Network.Connection     (Connection)
import           Arivi.Network.Types          (ConnectionId, NodeId,
                                               TransportType (..))
import           Arivi.P2P.Kademlia.Query
import           Arivi.P2P.MessageHandler
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Codec.Serialise              (deserialise, serialise)
import           Control.Concurrent
import           Control.Concurrent.Lifted    (fork)
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception            (SomeException, try)
import           Control.Monad                (forever, when)
import           Control.Monad.IO.Class       (liftIO)
import           Data.ByteString.Char8        as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy         as ByteStringLazy (ByteString,
                                                                 toStrict)

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
makeP2Pinstance :: NodeId -> String -> Int -> Int -> Float -> Int -> IO ()
makeP2Pinstance nodeId ip port minPeerCount outboundPeerQuota maxConnectionAllowed = do

    ariviP2PInstanceTvar <-  atomically( newTVar  ( AriviP2PInstance nodeId ip port
                                                     outboundPeerQuota maxConnectionAllowed) )

    -- create p2p environment and intitalize all the map empty
    p2p' <- makeP2PEnvironment
    let p2pEnv = p2p' {   tvarAriviP2PInstance = ariviP2PInstanceTvar
                        , minPeerCountPerTopic = minPeerCount
                      }

    liftIO $ forkIO $ runP2Papp p2pEnv (handleIncomingConnections outboundPeerQuota maxConnectionAllowed)
    return ()


-- Check if we have any incoming peers asking for subscription later add them if raito is not violated
handleIncomingConnections ::(HasP2PEnv m ) => Float -> Int -> m ()
handleIncomingConnections outboundPeerQuota maxConnectionAllowed = forever $ do

    -- Network layer Blocking Call *API* which gives ConnectionId of any incoming peers
    let connId = getNewConnection

    p2p <- getP2PEnv
    let ariviP2PInstanceTvar = tvarAriviP2PInstance p2p

    liftIO $ forkIO ( processIncomingFromConnection connId )


registerService :: (HasP2PEnv m) => ServiceCode -> [TopicCode] ->
                   MinPeerCountPerTopic -> TransportType -> NodeType -> m ()
registerService serviceCode topicCodeList minPeerCountPerTopic transport peerType = do

                -- map topictoservice and servicetotopic
                topicToServiceMapTvar <- getTopicToServiceMapP2PEnv
                liftIO $ insertIntoTopicToServiceMap topicToServiceMapTvar serviceCode topicCodeList

                -- register each topic provided in topicCodelist
                _ <- mapM_ (registerTopic  minPeerCountPerTopic transport peerType) topicCodeList
                return ()


insertIntoTopicToServiceMap :: TVar TopicToServiceMap -> ServiceCode -> [TopicCode] -> IO ()
insertIntoTopicToServiceMap topicToServiceMapTvar serviceCode topicCodeList =
    when (topicCodeList /= []) (do
        atomically( modifyTVar topicToServiceMapTvar (Map.insert  (head topicCodeList) serviceCode) )
        insertIntoTopicToServiceMap topicToServiceMapTvar serviceCode (tail topicCodeList) )


-- To newly register provided topic into TopicContextMap, SubscriptionMap and WatcherMap
registerTopic :: (HasP2PEnv m ) => MinPeerCountPerTopic -> TransportType -> NodeType -> TopicCode -> m ()
registerTopic minPeerCountPerTopic transport peerType topicCode = do

    ariviP2PInstanceTvar  <- getAriviTVarP2PEnv
    subscriptionMapTvar   <- getSubscriptionMapP2PEnv
    watchersMapTvar       <- getWatchersMapTVarP2PEnv
    topicContextMapTvar   <- getTopicContextMapP2PEnv

    (maxConnectionAllowed',outboundPeerQuota') <- liftIO $ do
        atomically( modifyTVar topicContextMapTvar (registerTopicInTopicCntxtTvar topicCode
                                                     minPeerCountPerTopic peerType transport))
        atomically( modifyTVar subscriptionMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))
        atomically( modifyTVar watchersMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))

        ariviP2PInstance <- liftIO $ atomically( readTVar ariviP2PInstanceTvar )
        return (maxConnectionAllowed ariviP2PInstance, outboundPeerQuota ariviP2PInstance)

    _ <- fork (addSubscriberThread topicCode minPeerCountPerTopic maxConnectionAllowed')

    return ()


-- TO BE USED IN modifyTVar to register a new Topic in the given TopicContextMap with required context details
registerTopicInTopicCntxtTvar :: TopicCode -> MinPeerCountPerTopic -> NodeType ->
    TransportType-> TopicContextMap -> TopicContextMap
registerTopicInTopicCntxtTvar topicCode minPeerCountPerTopic peerType transport topCntxt =
    let context = (minPeerCountPerTopic, peerType, transport)
    in  Map.insert topicCode context topCntxt

-- TO BE USED IN modifyTVar to register/initalize the new topic if it does not already existing in Subscription Map and Watchers Map
registerTopicSubMapOrWatchMapTvar topicCode subscriptionSubOrWatchMap =
    let hasTopicCode = Map.lookup topicCode subscriptionSubOrWatchMap
    in  case hasTopicCode of Nothing ->  Map.insert topicCode [] subscriptionSubOrWatchMap
                             _ -> subscriptionSubOrWatchMap

processIncomingFromConnection :: ConnectionId -> IO ()
processIncomingFromConnection connId = forever $

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

-- Check if addition of an inbound/Watcher violates the outboundPeerQuota raito
willViolateRaito :: (HasP2PEnv m) => TopicCode -> Float -> Int -> m Bool
willViolateRaito topicCode outboundPeerQuota maxConnectionAllowed = do

    subscriptionMapTvar   <- getSubscriptionMapP2PEnv
    watchersMapTvar       <- getWatchersMapTVarP2PEnv

    liftIO $ do
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
neededPeerCount :: TopicCode -> TopicContextMap -> SubscriptionMap -> WatchersMap -> Int
neededPeerCount topicCode topicContextMapMap subscriptionMap watchersMap =
    let
    -- In case lookup gives a nothing fromMaybe will take care of it
    topicContextMap   = Map.lookup topicCode topicContextMapMap
    subpeerlist    = fromMaybe [] (Map.lookup topicCode subscriptionMap)
    notifypeerlist = fromMaybe [] (Map.lookup topicCode watchersMap)

    peerlist = map getNodeIdFromContext subpeerlist ++  map  getNodeIdFromContext notifypeerlist
    totalconnectioncount = length $ Set.fromList  peerlist

    minimumPeerConnection = case topicContextMap of Nothing -> 0
                                                    Just (peerCount, _, _) -> peerCount
    in  minimumPeerConnection - totalconnectioncount


-- It checks minimum PeerConnection requirement and sends subscribe messages to Peer
addSubscriberThread :: (HasP2PEnv m ) => TopicCode -> MinPeerCountPerTopic -> Int -> m ()
addSubscriberThread topicCode minPeerCount maxConnectionAllowed = forever $ do
    watchersMapTvar       <- getWatchersMapTVarP2PEnv
    subscriptionMapTvar   <- getSubscriptionMapP2PEnv
    topicContextMapTvar   <- getTopicContextMapP2PEnv
    peerToTopicMapTvar <- getPeerToTopicMapP2PEnv

    liftIO $ do
        subscriptionMap   <- atomically( readTVar subscriptionMapTvar )
        watchersMap       <- atomically( readTVar watchersMapTvar )
        topicContextMap   <- atomically( readTVar topicContextMapTvar )

        let uniqueLen = length . Set.fromList . List.intercalate [] . Map.elems
        let subscriptionLen = uniqueLen subscriptionMap
        let totalCount = subscriptionLen + uniqueLen watchersMap

        let numOfPeerNeeded = neededPeerCount topicCode topicContextMap subscriptionMap watchersMap

        if totalCount < maxConnectionAllowed && numOfPeerNeeded > 0 then
            do
                _ <- sendSubscriptionToNPeers peerToTopicMapTvar topicCode numOfPeerNeeded

                -- wait for certain timeout 30 seconds before checking or sending subscribe message to a another peer
                threadDelay (30*1000000)
                return ()
        else
            do
                -- wait for certain timeout 100 seconds before checking or sending subscribe message to a another peer
                threadDelay (100*1000000)
                return ()

sendSubscriptionToNPeers ::  TVar PeerToTopicMap -> TopicCode -> Int -> IO ()
sendSubscriptionToNPeers peerToTopicMapTvar topicCode num = do
    -- get peer from topicToPeerMap
    peer <- getPeer topicCode peerToTopicMapTvar

    -- asking network layer gives connectionId first by checking if connection already exists and if not by forming a new one
    let connId = openConnection (peerNodeId peer) (peerIp peer) (peerPort peer) (peerTransportType peer)

    -- sending a subscribe message to kademila provided nodeId for given topic
    subscribeMessage connId topicCode
    when (num > 0) (sendSubscriptionToNPeers peerToTopicMapTvar topicCode (num-1) )

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
