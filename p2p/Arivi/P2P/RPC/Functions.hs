--{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.Functions where

import           Arivi.Network.Types                   (ConnectionId)
import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageCode (..),
                                                        P2PPayload, Peer (..),
                                                        TransportType (..))
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.Lifted             (fork)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (ByteString)
import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
                                                                toStrict)
import           Data.HashMap.Strict                   as HM
import           Data.Maybe

{-
  structure of HashMap entry => key : ResourceId, value : (ServiceId, TQueue Peers)
-}
-- registers all the resources requested by a service in a HashMap
registerResource :: ServiceId -> ResourceList -> TVar ResourceToPeerMap -> IO ()
registerResource _ [] _ = return () -- recursion corner case
registerResource serviceId (resource:resourceList) resourceToPeerMap = do
    peers <- newTQueueIO -- create a new empty Tqueue for Peers
    messagesRecieved <- newTQueueIO
    atomically -- read Tvar , update HashMap, write it back
        (do resourceToPeerMapTvar <- readTVar resourceToPeerMap --
            let temp =
                    HM.insert
                        resource
                        (serviceId, peers, messagesRecieved)
                        resourceToPeerMapTvar --
            writeTVar resourceToPeerMap temp)
    registerResource serviceId (resource : resourceList) resourceToPeerMap

-------------------- Functions for periodic updation of the hashmap ---------------------
-- creates a worker thread
-- thread should read the hashMap and should check if the number of peers for a resource is less than some number
-- if it is less should ask Kademlia for more nodes
-- send each peer and option message
-- the options message module will handle the sending of messages and updating of the HashMap based on the support message
checkPeerInResourceMap :: TVar ResourceToPeerMap -> IO ()
checkPeerInResourceMap resourceToPeerMapTvar = do
    let minimumPeers = 5
    forkIO (checkPeerInResourceMapHelper resourceToPeerMapTvar minimumPeers)
    return ()

-- need to integrate kademlia and SendOptions functions (in RPC/SendOptions.hs)
checkPeerInResourceMapHelper :: TVar ResourceToPeerMap -> Int -> IO ()
checkPeerInResourceMapHelper resourceToPeerMapTvar minimumPeers =
    forever $ do
        resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
        let tempList = HM.toList resourceToPeerMap
        listOfLengths <- extractListOfLengths tempList
        let numberOfPeers = minimumPeers - minimum listOfLengths
        if numberOfPeers > 0
            --askKademliaForPeers
            -- send options message
            then threadDelay (40 * 1000)
            else threadDelay (30 * 1000) -- in milliseconds
        return ()

-- function to find the TQueue with minimum length
-- used by the worker thread
extractListOfLengths ::
       [(ResourceId, (ServiceId, TQueue Peer, TQueue ServicePayload))]
    -> IO [Int]
extractListOfLengths [] = return [0]
extractListOfLengths (x:xs) = do
    let temp = extractSecond (snd x) -- get the TQueue of Peers
    len <-
        atomically
            (do listofTQ <- flushTQueue temp
                writeBackToTQueue temp listofTQ
                return (length listofTQ))
    lenNextTQ <- extractListOfLengths xs
    return $ len : lenNextTQ

-- write the Peers flushed from the TQueue back to the TQueue
writeBackToTQueue :: TQueue Peer -> [Peer] -> STM ()
writeBackToTQueue _ [] = return ()
writeBackToTQueue currTQ (currentElem:listOfTQ) = do
    writeTQueue currTQ currentElem
    writeBackToTQueue currTQ listOfTQ

-- DUMMY FUNCTION !!!
-- signature of the function to ask Kademlia for peers
askKademliaForPeers :: Int -> Peer -> [Peer]
askKademliaForPeers numberOfPeers peer = [peer]

------------- Functions for reading requests and responding to them -------------------
getResource ::
       (HasP2PEnv m) => NodeId -> ResourceId -> ServiceMessage -> m ByteString
getResource mynodeid resourceID servicemessage = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    --resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let temp = HM.lookup resourceID resourceToPeerMap
    let peerTQ = extractSecond (fromJust temp)
    getPeer peerTQ resourceID mynodeid servicemessage

getPeer ::
       (HasP2PEnv m)
    => TQueue Peer
    -> ResourceId
    -> NodeId
    -> ServiceMessage
    -> m ByteString
getPeer peerTQ resourceID mynodeid servicemessage = do
    peer <- liftIO $ atomically (readTQueue peerTQ)
    let tonodeid = nodeId peer
    let message1 =
            RequestRC
                { to = tonodeid
                , from = mynodeid
                , rid = resourceID -- add RID
                , serviceMessage = servicemessage
                }
    let message = Lazy.toStrict $ serialise message1
    res1 <- Exception.try $ sendRequest peer RPC message TCP
    case res1 of
        Left (e :: Exception.SomeException) ->
            getPeer peerTQ resourceID mynodeid servicemessage
        Right returnMessage -> do
            let inmessage =
                    deserialise (Lazy.fromStrict returnMessage) :: MessageTypeRPC
            let a = to inmessage
            let b = from inmessage
            let c = serviceMessage inmessage
            let d = rid inmessage
            if (mynodeid == a && tonodeid == b) && resourceID == d
                then liftIO $ atomically (writeTQueue peerTQ peer) >> return c
                else getPeer peerTQ resourceID mynodeid servicemessage

resourceRequestThread :: (HasP2PEnv m) => m ()
resourceRequestThread = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    fork (resourceRequestHelper resourceToPeerMap)
    return ()

resourceRequestHelper :: (HasP2PEnv m) => ResourceToPeerMap -> m ()
resourceRequestHelper resourceToPeerMap = do
    messageInfoRPC <- readRPCRequest
    let uuid = fst messageInfoRPC
    let rpcRequest =
            deserialise (Lazy.fromStrict $ snd messageInfoRPC) :: MessageTypeRPC
    -- check the to
    let fromPeer = from rpcRequest
    let resourceId = rid rpcRequest
    let message = serviceMessage rpcRequest
    let temp = HM.lookup resourceId resourceToPeerMap
    if isNothing temp
        then return ()
        else do
            let sid = extractFirst (fromJust temp) -- currently not needed Might use later
            let currTQ = extractThird (fromJust temp)
            -- need to put it in TQueue of Service
            let newEntry =
                    ServicePayload
                        resourceId
                        message
                        (Just (P2Pinfo uuid fromPeer))
            putIntoTQueue sid currTQ newEntry
            resourceRequestHelper resourceToPeerMap

sendResource :: (HasP2PEnv m) => ServicePayload -> NodeId -> m ()
sendResource servicemessage fromNodeId --we wont be passing from node id
                                            --to get the nodeid we use the environment variable
 = do
    let resourceId = resid servicemessage
    let extractedmessage = message servicemessage
    let extra_info = extra servicemessage
    let nodeInfo = fromJust extra_info
    let uuid1 = uuid nodeInfo
    let toNode = node nodeInfo
    let p2p_payload =
            ReplyRC
                { to = toNode
                , from = fromNodeId
                , rid = resourceId
                , serviceMessage = extractedmessage
                }
    let payload = Lazy.toStrict $ serialise p2p_payload
    {-
    sendResponse ::
       (HasP2PEnv m)
    => MessageInfo
    -> Peer
    -> TransportType
    -> MessageCode
    -> m ()
    -}
    let messageinfo = (uuid1, payload)
    --sendResponse messageinfo TCP RPC
    return ()

-- currently passing the TQ later need to find correct TQ using ServiceId
putIntoTQueue ::
       (HasP2PEnv m)
    => ServiceId
    -> TQueue ServicePayload
    -> ServicePayload
    -> m ()
putIntoTQueue sid tqueue newEntry =
    liftIO $ atomically (writeTQueue tqueue newEntry)

--dummy function
sendRequest1 :: Peer -> MessageCode -> P2PPayload -> TransportType -> ByteString
sendRequest1 peer mCode message transportType = do
    let inmessage = deserialise (Lazy.fromStrict message) :: MessageTypeRPC
    let message1 =
            ReplyRC
                { to = to inmessage
                , from = from inmessage
                , rid = rid inmessage
                , serviceMessage = serviceMessage inmessage
                }
    Lazy.toStrict $ serialise message1
