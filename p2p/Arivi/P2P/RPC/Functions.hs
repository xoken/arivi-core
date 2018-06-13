module Arivi.P2P.RPC.Functions
    ( module Arivi.P2P.RPC.Functions
    ) where

import           Arivi.Network.Types              (ConnectionId, NodeId,
                                                   TransportType (..))
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.RPC.Types
import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad                    (forever)
import           Control.Monad.STM
import           Data.HashMap.Strict              as HM
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
registerResource ::
       ServiceId -> ResourceList -> TVar ResourceToPeerMap -> IO () -- Maybe here is used to return nothing for now
registerResource _ [] _ = return ()
registerResource serviceId (resource:resourceList) resourceToPeerMap = do
    peers <- newTQueueIO -- create a new empty Tqueue
    atomically
        (do resourceToPeerMapTvar <- readTVar resourceToPeerMap --
            let temp =
                    HM.insert resource (serviceId, peers) resourceToPeerMapTvar
            writeTVar resourceToPeerMap temp)
    registerResource serviceId (resource : resourceList) resourceToPeerMap

-- worker thread
-- should read the hashMap
-- should check if the number of peers for a resource is less than some number
-- if it is less should ask kademlia for more nodes
-- send each peer and option message
-- do this for each peer
-- then for each entry in the hash map
--registerResource serviceCode resourceList
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
                                                                                                                        -- ask peers from kademlia
        if numberOfPeers > 0
                                                                                                                                        --askKademliaForPeers
                                                                                                                                        -- send options message
            then threadDelay (40 * 1000)
            else threadDelay (30 * 1000) -- in milliseconds
        return ()

-- function to find the Tqueue with minimum length
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

writeBackToTQueue :: TQueue Peer -> [Peer] -> STM ()
writeBackToTQueue _ [] = return ()
writeBackToTQueue currTQ (currentElem:listOfTQ) = do
    writeTQueue currTQ currentElem
    writeBackToTQueue currTQ listOfTQ

-- should eventually write into int option message module
-- basically option message module will handle the sending options and putting peers into HashMap
updateResourcePeers :: (Peer, [ResourceId]) -> TVar ResourceToPeerMap -> IO Int
updateResourcePeers peerResourceTuple resourceToPeerMapTvar = do
    resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let peer = fst peerResourceTuple
    let listOfResources = snd peerResourceTuple
    updateResourcePeersHelper peer listOfResources resourceToPeerMap

-- write a wrapper for this
-- adds the peer to the TQueue of each resource
-- lookup for the current resource in the HashMap
-- assumes that the resourceIDs are present in the HashMap
-- cannot add new currently because the serviceID is not available
updateResourcePeersHelper ::
       Peer -> [ResourceId] -> ResourceToPeerMap -> IO Int
updateResourcePeersHelper _ [] _ = return 0
updateResourcePeersHelper peer (currResource:listOfResources) resourceToPeerMap = do
    let temp = HM.lookup currResource resourceToPeerMap
                                                                                                                                                                -- check for lookup returning Nothing
    if isNothing temp
        then updateResourcePeersHelper peer listOfResources resourceToPeerMap
        else do
            let currTQ = snd (fromJust temp)
            atomically (writeTQueue currTQ peer)
            tmp <-
                updateResourcePeersHelper
                    peer
                    listOfResources
                    resourceToPeerMap
            return $ 1 + tmp

-- DUMMY FUNCTION !!!
askKademliaForPeers :: Int -> Peer -> [Peer]
askKademliaForPeers numberOfPeers peer = [peer]
{-
acceptRequest (ResourceID)
--read from ResourceID Tchan
return (serviceMessage,
-}
