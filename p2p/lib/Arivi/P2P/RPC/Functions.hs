--{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.Functions
    ( registerResource
    , getResource
    -- -- not for Service Layer
    , rpcHandler
    , updatePeerInResourceMap
    , addPeerFromKademlia
    ) where

import           Arivi.P2P.Exception
import qualified Arivi.P2P.Kademlia.Kbucket            as Kademlia (Peer (..), getKClosestPeersByNodeid,
                                                                    getKRandomPeers)
import qualified Arivi.P2P.Kademlia.Types              as KademliaTypes (NodeEndPoint (..))
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (Handle (..),
                                                        MessageType (..),
                                                        NodeIdPeerMap,
                                                        P2PPayload,
                                                        PeerDetails (..))
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (pack)
import           Data.ByteString.Lazy                  as Lazy (fromStrict)
import           Data.Either.Unwrap
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
-- import           Debug.Trace
-- import           System.Random                         (randomRIO)

getEntryBasedOnTypeOfResource ::
       Maybe (ResourceHandler, TVar [NodeId])
    -> Maybe (ResourceHandler, TVar [NodeId])
    -> Maybe (ResourceHandler, TVar [NodeId])
getEntryBasedOnTypeOfResource entryInArchivedResourceMap entryInTransientResourceMap
    | isJust entryInArchivedResourceMap = entryInArchivedResourceMap
    | isJust entryInTransientResourceMap = entryInTransientResourceMap
    | otherwise = Nothing

-- register the resource and it's handler in the ResourceToPeerMap of RPC
registerResource ::
       (HasP2PEnv m, HasLogging m)
    => ResourceId
    -> ResourceHandler
    -> ResourceType
    -> m ()
registerResource resource resourceHandler resourceType = do
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    nodeIds <- liftIO $ newTVarIO [] -- create a new empty Tqueue for Peers
    case resourceType of
        Archived ->
            liftIO $
            atomically
                (do archivedResourceToPeerMap <-
                        readTVar archivedResourceToPeerMapTvar
                    let newMap =
                            HM.insert
                                resource
                                (resourceHandler, nodeIds)
                                archivedResourceToPeerMap --
                    writeTVar archivedResourceToPeerMapTvar newMap)

        Transient ->
            liftIO $
            atomically
                (do transientResourceToPeerMap <-
                        readTVar transientResourceToPeerMapTVar
                    let updatedMap =
                            HM.insert
                                resource
                                (resourceHandler, nodeIds)
                                transientResourceToPeerMap --
                    writeTVar transientResourceToPeerMapTVar updatedMap)

-- TODO : need to replace currNodeId passed with nodeId from environment
-- TODO : integrate dynamic and static resource hashmaps
-------------------- Functions for periodic updation of the hashmap ---------------------
-- creates a worker thread
-- thread should read the hashMap and should check if the number of peers for a resource is less than some number
-- if it is less should ask Kademlia for more nodes
-- send each peer and option message
-- the options message module will handle the sending of messages and updating of the HashMap based on the support message
updatePeerInResourceMap :: (HasP2PEnv m, HasLogging m) => m ()
updatePeerInResourceMap = do
    currNodeId <- getSelfNodeId
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    let minimumNodes = 5
    _ <-
        LAsync.async
            (updatePeerInResourceMapHelper
                 archivedResourceToPeerMap
                 minimumNodes
                 currNodeId)
    return ()

updatePeerInResourceMapHelper ::
       (HasP2PEnv m, HasLogging m)
    => ArchivedResourceToPeerMap
    -> Int
    -> NodeId
    -> m ()
updatePeerInResourceMapHelper archivedResourceToPeerMap minimumNodes currNodeId =
    forever $ do
        let tempList = HM.toList archivedResourceToPeerMap
        listOfLengths <- liftIO $ extractListOfLengths tempList
        let numberOfPeers = minimumNodes - minimum listOfLengths
        if numberOfPeers > 0
            then do
                peerRandom <- Kademlia.getKRandomPeers 2
                res1 <-
                    Exception.try $
                    Kademlia.getKClosestPeersByNodeid currNodeId 3
                peersClose <-
                    case res1 of
                        Left (_ :: Exception.SomeException) ->
                            Kademlia.getKRandomPeers 3 -- decide properly
                        Right peers -> return (fromRight peers)
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                sendOptionsMessage currNodeId nodeIds
                liftIO $ threadDelay (40 * 1000000)
            else liftIO $ threadDelay (30 * 1000000) -- in microseconds
        return ()

-- function to find the TQueue with minimum length
-- used by the worker thread
extractListOfLengths ::
       [(ResourceId, (ResourceHandler, TVar [NodeId]))] -> IO [Int]
extractListOfLengths [] = return [0]
extractListOfLengths (x:xs) = do
    let nodeListTVar = snd (snd x) -- get the TQueue of Peers
    len <- atomically (length <$> readTVar nodeListTVar)
    lenNextTQ <- extractListOfLengths xs
    return $ len : lenNextTQ

-- -- write the Peers flushed from the TQueue back to the TQueue
-- writeBackToTQueue :: TQueue NodeId -> [NodeId] -> STM ()
-- writeBackToTQueue _ [] = return ()
-- writeBackToTQueue currTQ (currentElem:listOfTQ) = do
--     writeTQueue currTQ currentElem
--     writeBackToTQueue currTQ listOfTQ
--
-----------------------
-- get NodeId from environment
getResource ::
       (HasP2PEnv m, HasLogging m)
    => ResourceId
    -> ServiceMessage
    -> m ServiceMessage
getResource resourceID servicemessage = do
    mynodeid <- getSelfNodeId
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    transientResourceToPeerMap <-
        liftIO $ readTVarIO transientResourceToPeerMapTVar
    --resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let entryInArchivedResourceMap =
            HM.lookup resourceID archivedResourceToPeerMap

    let entryInTransientResourceMap =
            HM.lookup resourceID transientResourceToPeerMap
    let entry =
            getEntryBasedOnTypeOfResource
                entryInArchivedResourceMap
                entryInTransientResourceMap
    if isNothing entry
        then return $ Lazy.fromStrict $ pack "Entry not found Error"
        else do
            let nodeListTVar = snd (fromJust entry)
            nodeList <- liftIO $ atomically $ readTVar nodeListTVar
            liftIO $ print nodeList
    -- should check if list is empty
            sendResourceRequestToPeer
                nodeListTVar
                resourceID
                mynodeid
                servicemessage

sendResourceRequestToPeer ::
       (HasP2PEnv m, HasLogging m)
    => TVar [NodeId]
    -> ResourceId
    -> NodeId
    -> ServiceMessage
    -> m ServiceMessage
sendResourceRequestToPeer nodeListTVar resourceID mynodeid servicemessage = do
    nodeList <- liftIO $ readTVarIO nodeListTVar
    -- mNodeId <- liftIO $ (nodeList !!) <$> randomRIO (0, length nodeList - 1) -- selecting a random node
    let mNodeId = head nodeList
    let requestMessage =
            RequestResource
                { to = mNodeId
                , from = mynodeid
                , rid = resourceID -- add RID
                , serviceMessage = servicemessage
                }
    let mMessage = serialise requestMessage
    res1 <- Exception.try $ sendRequest mNodeId RPC mMessage
    case res1 of
        Left (_ :: Exception.SomeException) ->
            sendResourceRequestToPeer
                nodeListTVar
                resourceID
                mynodeid
                servicemessage -- should discard the peer
        Right returnMessage -> do
            let inmessage = deserialise returnMessage :: MessageTypeRPC
            case inmessage of
                ReplyResource toNodeId fromNodeId resID _ ->
                    if (mynodeid == toNodeId && mNodeId == fromNodeId) &&
                       resourceID == resID
                        then liftIO $ return (serviceMessage inmessage)
                        else sendResourceRequestToPeer
                                 nodeListTVar
                                 resourceID
                                 mynodeid
                                 servicemessage
                Response _ _ responseCode' ->
                    case responseCode' of
                        Busy ->
                            sendResourceRequestToPeer
                                nodeListTVar
                                resourceID
                                mynodeid
                                servicemessage
                        Error -> return $ Lazy.fromStrict $ pack " error " -- need to define proper error handling maybe throw an exception
 -- should check to and from
                _ -> return $ Lazy.fromStrict $ pack " default"

-- will need the from NodeId to check the to and from
-- rpcHandler :: (HasP2PEnv m) => NodeId -> P2PPayload -> P2PPayload
rpcHandler :: (HasP2PEnv m) => P2PPayload -> m P2PPayload
rpcHandler incomingRequest = do
    let request = deserialise incomingRequest :: MessageTypeRPC
    case request of
        RequestResource myNodeId mNodeId resourceId requestServiceMessage -> do
            archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
            archivedResourceToPeerMap <-
                liftIO $ readTVarIO archivedResourceToPeerMapTvar
            transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
            transientResourceToPeerMap <-
                liftIO $ readTVarIO transientResourceToPeerMapTVar
            let entryInArchivedResourceMap =
                    HM.lookup resourceId archivedResourceToPeerMap
            let entryInTransientResourceMap =
                    HM.lookup resourceId transientResourceToPeerMap
            let entry =
                    getEntryBasedOnTypeOfResource
                        entryInArchivedResourceMap
                        entryInTransientResourceMap
            if isNothing entry
                then return $ Lazy.fromStrict $ pack "Entry not found Error"
                else do
                    let resourceHandler = fst $ fromJust entry
                    let responseServiceMessage =
                            resourceHandler requestServiceMessage
                    let replyMessage =
                            ReplyResource
                                { to = mNodeId
                                , from = myNodeId
                                , rid = resourceId
                                , serviceMessage = responseServiceMessage
                                }
                    let rpcResponse = serialise replyMessage
                    return rpcResponse
            -- currently catching everything and returning an empty byte string in future need to define proper error messages
        _ -> throw HandlerNotRequest

-- should take peer from kademlia instead of taking stuff separately
-- do it while integrating PeerDetails
-- also need to integrate with the environment
-- final type signature =   addPeerFromKademlia :: Peer -> m()
-- let peerDetailsTVar = fromJust (HM.lookup nodeIdPeer nodeIdMap)
-- peerDetails <- readTVar peerDetailsTVar
-- return ()
addPeerFromKademlia ::
       (HasP2PEnv m, HasLogging m) => [Kademlia.Peer] -> m [NodeId]
addPeerFromKademlia [] = return []
addPeerFromKademlia (peer:peerList) = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nextNodeId <- addPeerFromKademlia peerList
    mNodeId <- addPeerFromKademliaHelper peer nodeIdMapTVar
    return $ mNodeId : nextNodeId

addPeerFromKademliaHelper ::
       (HasP2PEnv m, HasLogging m)
    => Kademlia.Peer
    -> TVar NodeIdPeerMap
    -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar = do
    uuidMapTVar <- liftIO $ newTVarIO HM.empty
    liftIO $
        atomically
            (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
                let mNodeId = fst $ Kademlia.getPeer peerFromKademlia
                let kadNodeEndPoint = snd $ Kademlia.getPeer peerFromKademlia
                let mapEntry = HM.lookup mNodeId nodeIdPeerMap
                let newUdpPort = Just (KademliaTypes.udpPort kadNodeEndPoint)
                let newTcpPort = Just (KademliaTypes.tcpPort kadNodeEndPoint)
                do if isNothing mapEntry
                       then do
                           let newDetails =
                                   PeerDetails
                                       { nodeId = mNodeId
                                       , rep = Nothing
                                       , ip = Nothing -- needs to be changes once kademlia type is fixed
                                       , udpPort = newUdpPort
                                       , tcpPort = newTcpPort
                                       , streamHandle = NotConnected
                                       , datagramHandle = NotConnected
                                       , tvarUUIDMap = uuidMapTVar
                                       }
                           newPeerTvar <- newTVar newDetails
                           let newHashMap =
                                   HM.insert mNodeId newPeerTvar nodeIdPeerMap
                           writeTVar nodeIdPeerMapTVar newHashMap
                       else do
                           oldPeerDetails <- readTVar (fromJust mapEntry)
                            -- ip needs to be changes once kademlia type is fixed
                           let newDetails =
                                   oldPeerDetails
                                       { ip = Nothing
                                       , udpPort = newUdpPort
                                       , tcpPort = newTcpPort
                                       }
                           writeTVar (fromJust mapEntry) newDetails
                   return mNodeId)
-- used by PubSub to register peer for dynamic resource, which it has been notified for
-- readResourceRequest :: (HasP2PEnv m) => ResourceId -> m ServicePayload
-- readResourceRequest resourceId = do
--     resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
--     resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
--     let temp = HM.lookup resourceId resourceToPeerMap
--     let messageTQ = extractThird $ fromJust temp
--     liftIO $ atomically (readTQueue messageTQ)
--we wont be passing from node id
--we get the nodeid we use the environment variable
-- sendResource :: (HasP2PEnv m) => ServicePayload -> NodeId -> m ()
-- sendResource servicemessage fromNodeId = do
--     let resourceId = resid servicemessage
--     let extractedmessage = message servicemessage
--     let extra_info = extra servicemessage
--     let nodeInfo = fromJust extra_info
--     let uuid1 = uuid nodeInfo
--     let toNode = node nodeInfo
--     let p2p_payload =
--             ReplyResource
--                 { to = toNode
--                 , from = fromNodeId
--                 , rid = resourceId
--                 , serviceMessage = extractedmessage
--                 }
--     let payload = Lazy.toStrict $ serialise p2p_payload
--     let messageInfo = (uuid1, payload)
--     sendResponse toNode messageInfo RPC
------------- Functions for reading requests and adding them to Respective TQueues -------------------
-- updateResourceTQueueThread :: (HasP2PEnv m) => m ()
-- updateResourceTQueueThread = do
--     resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
--     resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
--     fork (updateResourceTQueueHelper resourceToPeerMap)
--     return ()
-- updateResourceTQueueHelper :: (HasP2PEnv m) => ResourceToPeerMap -> m ()
-- updateResourceTQueueHelper resourceToPeerMap = do
--     messageInfoRPC <- readRPCRequest
--     let uuid = fst messageInfoRPC
--     let rpcRequest =
--             deserialise (Lazy.fromStrict $ snd messageInfoRPC) :: MessageTypeRPC
--     -- check thesupportMessage to
--     let fromPeer = from rpcRequest
--     let resourceId = rid rpcRequest
--     let message = serviceMessage rpcRequest
--     let temp = HM.lookup resourceId resourceToPeerMap
--     if isNothing temp
--         then return ()
--         else do
--             let sid = extractFirst (fromJust temp)
--             let currTQ = extractThird (fromJust temp) -- get the TQueue of pending messages
--             -- need to put it in TQueue of Service
--             let newEntry =
--                     ServicePayload
--                         resourceId
--                         message
--                         (Just (P2Pinfo uuid fromPeer))
--             liftIO $ atomically (writeTQueue currTQ newEntry) -- write the new request into the TQueue
--             updateResourceTQueueHelper resourceToPeerMap
{-
  structure of HashMap entry => key : ResourceId, value : (ServiceId, TQueue Peers)
-}
-- registers all the resources requested by a service in a HashMap
-- registerResource :: (HasP2PEnv m) => ServiceId -> ResourceList -> m ()
-- registerResource _ [] = return () -- recursion corner case
-- registerResource serviceId (resource:resourceList) = do
--     resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
--     resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
--     nodeIds <- liftIO newTQueueIO -- create a new empty Tqueue for Peers
--     messagesRecieved <- liftIO newTQueueIO -- create a new empty TQueue for incoming resource requests
--     liftIO $
--         atomically
--             (do let temp =
--                         HM.insert
--                             resource
--                             (serviceId, nodeIds, messagesRecieved)
--                             resourceToPeerMap --
--                 writeTVar resourceToPeerMapTvar temp)
--     registerResource serviceId (resource : resourceList)
