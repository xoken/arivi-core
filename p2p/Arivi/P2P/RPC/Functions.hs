--{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.Functions where

import           Arivi.Network.Types                   (ConnectionId,
                                                        TransportType (..))
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageCode (..),
                                                        P2PPayload, Peer (..))
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
    peers <- newTQueueIO -- create a new empty Tqueue
    atomically -- read Tvar , update HashMap, write it back
        (do resourceToPeerMapTvar <- readTVar resourceToPeerMap --
            let temp =
                    HM.insert resource (serviceId, peers) resourceToPeerMapTvar --
            writeTVar resourceToPeerMap temp)
    registerResource serviceId (resource : resourceList) resourceToPeerMap

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
extractListOfLengths :: [(ResourceId, (ServiceId, TQueue Peer))] -> IO [Int]
extractListOfLengths [] = return [0]
extractListOfLengths (x:xs) = do
    let temp = snd (snd x)
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

{-
acceptRequest (ResourceID)
--read from ResourceID TChan
return (serviceMessage,
-}
{-
--getResource(resourceID serviceMessage)
-- form messageType
--getPeer from ResourcetoPeerList
--ret = sendRequest peer messageType Tcp
-- check ret format with try catch
--if not good then return getResource(resourceID serviceMessage)
-- else return (serviceMessage ret)
-}
getResource ::
       (HasP2PEnv m)
    => NodeId
    -> ResourceId
    -> TVar ResourceToPeerMap
    -> ByteString
    -> m ByteString
getResource mynodeid resourceID resourceToPeerMapTvar servicemessage = do
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    --resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let temp = HM.lookup resourceID resourceToPeerMap
    let peerTQ = snd (fromJust temp)
    getPeer peerTQ resourceID mynodeid servicemessage

getPeer ::
       (HasP2PEnv m)
    => TQueue Peer
    -> ResourceId
    -> NodeId
    -> ByteString
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
    {-
sendOptionsToPeer :: (HasP2PEnv m) => Peer -> Peer -> m ()
sendOptionsToPeer sendingPeer recievingPeer = do
    let message = Options {to = nodeId recievingPeer, from = nodeId sendingPeer}
    let byteStringMessage = Lazy.toStrict $ serialise message
    res1 <- Exception.try $ sendRequest recievingPeer RPC byteStringMessage UDP -- not exactly RPC needs to be changed
    case res1 of
        Left (e :: Exception.SomeException) -> return ()
        Right returnMessage -> do
            let supportMessage =
                    deserialise (Lazy.fromStrict returnMessage) :: MessageTypeRPC
            case supportMessage of
                Support _ fromPeer resourceList ->
                    Control.Monad.when (to message == fromPeer) $
                    updateResourcePeers (recievingPeer, resourceList)
                _ -> return () -- should handle this better
    -}
    let a = sendRequest1 peer RPC message TCP
    let inmessage = deserialise (Lazy.fromStrict message) :: MessageTypeRPC
    let d = to inmessage
    let b = from inmessage
    let c = serviceMessage inmessage
    let e = rid inmessage
    if (mynodeid == d && tonodeid == b) && resourceID == e
                --writeTQueue :: TQueue a -> a -> STM ()
        then liftIO $
             atomically (writeTQueue peerTQ peer) >>
                --writeTVar resourceToPeerMap temp
             return c
        else getPeer peerTQ resourceID mynodeid servicemessage

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
