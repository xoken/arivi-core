{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    ) where

import           Arivi.Network.Types                   (ConnectionId)
import           Arivi.P2P.Kademlia.Utils              (extractFirst,
                                                        extractSecond,
                                                        extractThird)
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageInfo,
                                                        MessageType (..),
                                                        P2PPayload,
                                                        PeerDetails (..),
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

--This function will send the options message to all the peers in [NodeId] on separate threads
--This is the top level function that will be exposed
sendOptionsMessage :: (HasP2PEnv m) => NodeId -> [NodeId] -> m ()
sendOptionsMessage _ [] = return ()
sendOptionsMessage sendingPeer (recievingPeer:peerList) = do
    fork (sendOptionsToPeer sendingPeer recievingPeer)
    sendOptionsMessage sendingPeer peerList

-- this function runs on each lightweight thread
-- two major functions
-- 1. Formulate and send options message
-- 2. Update the hashMap based oh the supported message returned
-- blocks while waiting for a response from the Other Peer
sendOptionsToPeer :: (HasP2PEnv m) => NodeId -> NodeId -> m ()
sendOptionsToPeer sendingPeerNodeId recievingPeerNodeId = do
    let message = Options {to = recievingPeerNodeId, from = sendingPeerNodeId}
    let byteStringMessage = Lazy.toStrict $ serialise message
    res1 <-
        Exception.try $ sendRequest recievingPeerNodeId Option byteStringMessage -- not exactly RPC, needs to be changed
    case res1 of
        Left (e :: Exception.SomeException) -> return ()
        Right returnMessage -> do
            let supportMessage =
                    deserialise (Lazy.fromStrict returnMessage) :: MessageTypeRPC
            case supportMessage of
                Support _ fromPeer resourceList ->
                    Control.Monad.when (to message == fromPeer) $
                    updateResourcePeers (recievingPeerNodeId, resourceList)
                _ -> return () -- should handle this better

-- this wrapper will update the hashMap based on the supported message returned by the peer
updateResourcePeers :: (HasP2PEnv m) => (NodeId, [ResourceId]) -> m ()
updateResourcePeers peerResourceTuple = do
    resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
    resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
    let node = fst peerResourceTuple
    let listOfResources = snd peerResourceTuple
    liftIO $ updateResourcePeersHelper node listOfResources resourceToPeerMap
    return ()

-- adds the peer to the TQueue of each resource
-- lookup for the current resource in the HashMap
-- assumes that the resourceIDs are present in the HashMap
-- cannot add new currently because the serviceID is not available
updateResourcePeersHelper ::
       NodeId -> [ResourceId] -> ResourceToPeerMap -> IO Int
updateResourcePeersHelper _ [] _ = return 0
updateResourcePeersHelper nodeId (currResource:listOfResources) resourceToPeerMap = do
    let temp = HM.lookup currResource resourceToPeerMap -- check for lookup returning Nothing
    if isNothing temp
        then updateResourcePeersHelper nodeId listOfResources resourceToPeerMap
        else do
            let currTQ = snd (fromJust temp)
            atomically (writeTQueue currTQ nodeId)
            tmp <-
                updateResourcePeersHelper
                    nodeId
                    listOfResources
                    resourceToPeerMap
            return $ 1 + tmp

-- | takes an options message and returns a supported message
optionsHandler :: (HasP2PEnv m) => P2PPayload -> m P2PPayload
optionsHandler payload = do
    let optionsMessage = deserialise (Lazy.fromStrict payload) :: MessageTypeRPC
    case optionsMessage of
        Options myNodeId fromNodeId -> do
            resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
            resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
            let resourceList = keys resourceToPeerMap
            let message =
                    Support
                        { to = fromNodeId
                        , from = myNodeId
                        , supportedResources = resourceList
                        }
            let byteStringSupportMessage = Lazy.toStrict $ serialise message
            return byteStringSupportMessage
            -- need to have proper error/exception messages and handling
        _ -> return $ pack " "
-- -- Formulate and send the Supported message as a reply to the Options message
-- sendSupportedMessage :: (HasP2PEnv m) => MessageInfo -> NodeId -> NodeId -> m ()
-- sendSupportedMessage messageInfo sendingPeerNodeId recievingPeerNodeId = do
--     resourceToPeerMapTvar <- getResourceToPeerMapP2PEnv
--     resourceToPeerMap <- liftIO $ readTVarIO resourceToPeerMapTvar
--     let resourceList = keys resourceToPeerMap
--     let message =
--             Support
--                 { to = recievingPeerNodeId
--                 , from = sendingPeerNodeId
--                 , supportedResources = resourceList
--                 }
--     let byteStringMessage = Lazy.toStrict $ serialise message
--     let uuid = fst messageInfo
--     let newMessageInfo = (uuid, byteStringMessage)
--                 -- need to handle exceptions
--     sendResponse recievingPeerNodeId newMessageInfo Option -- might not be RPC
