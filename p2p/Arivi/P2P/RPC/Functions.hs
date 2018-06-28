--{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.Functions
    ( registerResource
    , getResource
    -- not for Service Layer
    , rpcHandler
    , updatePeerInResourceMap
    ) where

import           Arivi.Network.Types                   (ConnectionId)
import qualified Arivi.P2P.Kademlia.Kbucket            as Kademlia (Peer (..), getKClosestPeersByNodeid,
                                                                    getKRandomPeers)
import qualified Arivi.P2P.Kademlia.Types              as KademliaTypes (NodeEndPoint (..),
                                                                         nodeId,
                                                                         nodeIp)
import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (Handle (..), IP,
                                                        MessageType (..),
                                                        NodeIdPeerMap (..),
                                                        P2PPayload,
                                                        PeerDetails (..), Port,
                                                        TransportType (..))
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.RPC.Types
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent                    (forkIO, threadDelay)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.Lifted             (fork)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (ByteString,
                                                                 pack)
import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
                                                                toStrict)
import           Data.Either.Unwrap
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe

registerResource :: (HasP2PEnv m) => ResourceHandlerList -> m ()
registerResource [] = return () -- recursion corner case
registerResource (resourceTuple:resourceHandlerList) = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    nodeIds <- liftIO newTQueueIO -- create a new empty Tqueue for Peers
    let rid = fst resourceTuple
    let resourceHandler = snd resourceTuple
    liftIO $
        atomically
            (do resourceToPeerMap <- readTVar resourceToPeerMapTvar
                let newMap =
                        HM.insert
                            rid
                            (resourceHandler, nodeIds)
                            resourceToPeerMap --
                writeTVar resourceToPeerMapTvar newMap)
    registerResource resourceHandlerList

-- TODO : need to replace currNodeId passed with nodeId from environment
-------------------- Functions for periodic updation of the hashmap ---------------------
-- creates a worker thread
-- thread should read the hashMap and should check if the number of peers for a resource is less than some number
-- if it is less should ask Kademlia for more nodes
-- send each peer and option message
-- the options message module will handle the sending of messages and updating of the HashMap based on the support message
updatePeerInResourceMap :: (HasP2PEnv m) => NodeId -> m ()
updatePeerInResourceMap currNodeId = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    let minimumNodes = 5
    LAsync.async
        (updatePeerInResourceMapHelper resourceToPeerMap minimumNodes currNodeId)
    return ()

updatePeerInResourceMapHelper ::
       (HasP2PEnv m) => ResourceToPeerMap -> Int -> NodeId -> m ()
updatePeerInResourceMapHelper resourceToPeerMap minimumNodes currNodeId =
    forever $ do
        let tempList = HM.toList resourceToPeerMap
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
                        Left (e :: Exception.SomeException) ->
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
       [(ResourceId, (ResourceHandler, TQueue NodeId))] -> IO [Int]
extractListOfLengths [] = return [0]
extractListOfLengths (x:xs) = do
    let temp = snd (snd x) -- get the TQueue of Peers
    len <-
        atomically
            (do listofTQ <- flushTQueue temp
                writeBackToTQueue temp listofTQ
                return (length listofTQ))
    lenNextTQ <- extractListOfLengths xs
    return $ len : lenNextTQ

-- write the Peers flushed from the TQueue back to the TQueue
writeBackToTQueue :: TQueue NodeId -> [NodeId] -> STM ()
writeBackToTQueue _ [] = return ()
writeBackToTQueue currTQ (currentElem:listOfTQ) = do
    writeTQueue currTQ currentElem
    writeBackToTQueue currTQ listOfTQ

-----------------------
-- get NodeId from environment
getResource ::
       (HasP2PEnv m)
    => NodeId
    -> ResourceId
    -> ServiceMessage
    -> m ServiceMessage
getResource mynodeid resourceID servicemessage = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    --resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let temp = HM.lookup resourceID resourceToPeerMap
    let nodeTQ = snd (fromJust temp)
    sendResourceRequestToPeer nodeTQ resourceID mynodeid servicemessage

sendResourceRequestToPeer ::
       (HasP2PEnv m)
    => TQueue NodeId
    -> ResourceId
    -> NodeId
    -> ServiceMessage
    -> m ServiceMessage
sendResourceRequestToPeer nodeTQ resourceID mynodeid servicemessage = do
    nodeId <- liftIO $ atomically (readTQueue nodeTQ)
    let requestMessage =
            RequestResource
                { to = nodeId
                , from = mynodeid
                , rid = resourceID -- add RID
                , serviceMessage = servicemessage
                }
    let message = Lazy.toStrict $ serialise requestMessage
    res1 <- Exception.try $ sendRequest nodeId RPC message
    case res1 of
        Left (e :: Exception.SomeException) ->
            sendResourceRequestToPeer nodeTQ resourceID mynodeid servicemessage -- should discard the peer
        Right returnMessage -> do
            let inmessage =
                    deserialise (Lazy.fromStrict returnMessage) :: MessageTypeRPC
            case inmessage of
                ReplyResource toNodeId fromNodeId resID _ ->
                    if (mynodeid == toNodeId && nodeId == fromNodeId) &&
                       resourceID == resID
                        then liftIO $
                             atomically (unGetTQueue nodeTQ nodeId) >>
                             return (serviceMessage inmessage)
                        else sendResourceRequestToPeer
                                 nodeTQ
                                 resourceID
                                 mynodeid
                                 servicemessage
                Response _ _ responseCode ->
                    case responseCode of
                        Busy -> do
                            liftIO $ atomically (writeTQueue nodeTQ nodeId)
                            sendResourceRequestToPeer
                                nodeTQ
                                resourceID
                                mynodeid
                                servicemessage
                        Error -> return $ pack " error " -- need to define proper error handling maybe throw an exception
 -- should check to and from

-- will need the from NodeId to check the to and from
-- rpcHandler :: (HasP2PEnv m) => NodeId -> P2PPayload -> P2PPayload
rpcHandler :: (HasP2PEnv m) => P2PPayload -> m P2PPayload
rpcHandler incomingRequest = do
    let request =
            deserialise (Lazy.fromStrict incomingRequest) :: MessageTypeRPC
    case request of
        RequestResource myNodeId nodeId resourceId requestServiceMessage -> do
            resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
            resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
            let resourceTuple = HM.lookup resourceId resourceToPeerMap
            let resourceHandler = fst $ fromJust resourceTuple
            let responseServiceMessage = resourceHandler requestServiceMessage
            let replyMessage =
                    ReplyResource
                        { to = nodeId
                        , from = myNodeId
                        , rid = resourceId
                        , serviceMessage = requestServiceMessage
                        }
            let rpcResponse = Lazy.toStrict $ serialise replyMessage
            return rpcResponse
            -- currently catching everything and returning an empty byte string in future need to define proper error messages
        _ -> return $ pack " "

-- should take peer from kademlia instead of taking stuff separately
-- do it while integrating PeerDetails
-- also need to integrate with the environment
-- final type signature =   addPeerFromKademlia :: Peer -> m()
-- let peerDetailsTVar = fromJust (HM.lookup nodeIdPeer nodeIdMap)
-- peerDetails <- readTVar peerDetailsTVar
-- return ()
addPeerFromKademlia :: (HasP2PEnv m) => [Kademlia.Peer] -> m [NodeId]
addPeerFromKademlia [] = return []
addPeerFromKademlia (peer:peerList) = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nextNodeId <- addPeerFromKademlia peerList
    nodeId <- addPeerFromKademliaHelper peer nodeIdMapTVar
    return $ nodeId : nextNodeId

addPeerFromKademliaHelper ::
       (HasP2PEnv m) => Kademlia.Peer -> TVar NodeIdPeerMap -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar = do
    uuidMapTVar <- liftIO $ newTVarIO HM.empty
    liftIO $
        atomically
            (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
                let nodeId = fst $ Kademlia.getPeer peerFromKademlia
                let kadNodeEndPoint = snd $ Kademlia.getPeer peerFromKademlia
                let mapEntry = HM.lookup nodeId nodeIdPeerMap
                let newUdpPort = Just (KademliaTypes.udpPort kadNodeEndPoint)
                let newTcpPort = Just (KademliaTypes.tcpPort kadNodeEndPoint)
                do if isNothing mapEntry
                       then do
                           let newDetails =
                                   PeerDetails
                                       { nodeId = nodeId
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
                                   HM.insert nodeId newPeerTvar nodeIdPeerMap
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
                   return nodeId)
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
