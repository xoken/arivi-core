{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.SendOptions where

import           Arivi.Network.Types                   (ConnectionId, NodeId)
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageCode (..),
                                                        MessageInfo, Peer (..),
                                                        TransportType (..))
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Exception
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.Lifted             (fork)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever, when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (ByteString,
                                                                 pack, unpack)
import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
                                                                toStrict)
import           Data.HashMap.Strict                   as HM
import           Data.Maybe

--This function will send the options message to all the peers in [Peer] on separate threads
--This is the top level function that will be exposed
sendOptionsMessage :: (HasP2PEnv m) => Peer -> [Peer] -> m ()
sendOptionsMessage _ [] = return ()
sendOptionsMessage sendingPeer (recievingPeer:peerList) = do
    fork (sendOptionsToPeer sendingPeer recievingPeer)
    sendOptionsMessage sendingPeer peerList

-- this function runs on each lightweight thread
-- two major functions
-- 1. Formulate and send options message
-- 2. Update the hashMap based oh the supported message returned
-- blocks while waiting for a response from the Other Peer
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
                    Control.Monad.when $
                    (to message == fromPeer)
                        updateResourcePeers
                        (recievingPeer, resourceList)
                _ -> return () -- should handle this better

-- this wrapper will update the hashMap based on the supported message returned by the peer
updateResourcePeers :: (HasP2PEnv m) => (Peer, [ResourceId]) -> m ()
updateResourcePeers peerResourceTuple = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    let peer = fst peerResourceTuple
    let listOfResources = snd peerResourceTuple
    liftIO $ updateResourcePeersHelper peer listOfResources resourceToPeerMap
    return ()

-- adds the peer to the TQueue of each resource
-- lookup for the current resource in the HashMap
-- assumes that the resourceIDs are present in the HashMap
-- cannot add new currently because the serviceID is not available
updateResourcePeersHelper :: Peer -> [ResourceId] -> ResourceToPeerMap -> IO Int
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
                updateResourcePeersHelper peer listOfResources resourceToPeerMap
            return $ 1 + tmp

-- Formulate and send the Supported message as a reply to the Options message
sendSupportedMessage :: (HasP2PEnv m) => MessageInfo -> Peer -> Peer -> m ()
sendSupportedMessage messageInfo sendingPeer recievingPeer = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    let resourceList = keys resourceToPeerMap
    let message =
            Support
                { to = nodeId recievingPeer
                , from = nodeId sendingPeer
                , supportedResources = resourceList
                }
    let byteStringMessage = Lazy.toStrict $ serialise message
    let uuid = fst messageInfo
    let newMessageInfo = (uuid, byteStringMessage)
                -- need to handle exceptions
    sendResponse newMessageInfo recievingPeer UDP RPC -- might not be RPC
