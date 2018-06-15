{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.Functions where

import           Arivi.Network.Types                   (ConnectionId, NodeId,
                                                        TransportType (..))
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (Peer (..))
import           Arivi.P2P.RPC.Types
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad                         (forever)
import           Control.Monad.STM
import           Data.HashMap.Strict                   as HM
import           Data.Maybe

{-
--getResource(resourceID serviceMessage)
-- form messageType
--getPeer from ResourcetoPeerList
--ret = sendRequest peer messageType Tcp
-- check ret format with try catch
--if not good then return getResource(resourceID serviceMessage)
-- else return (serviceMessage ret)
-}
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
