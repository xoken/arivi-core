{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    , optionsHandler
    ) where

-- import           Arivi.Network.Types                   (ConnectionId)
-- import           Arivi.P2P.Kademlia.Utils              (extractFirst,
--                                                         extractSecond,
--                                                         extractThird)
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes (MessageType (..),
                                                        P2PPayload)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialiseOrFail,
                                                        serialise)

-- import           Control.Concurrent                    (forkIO, threadDelay)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)

-- import           Control.Concurrent.Lifted             (fork)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.STM
import           Data.ByteString.Char8                 as Char8 (pack)
                                                                --  pack, unpack)
                                                                -- toStrict)

-- import           Data.ByteString.Char8                 as Char8 (ByteString,
-- import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
import           Data.HashMap.Strict                   as HM

--This function will send the options message to all the peers in [NodeId] on separate threads
--This is the top level function that will be exposed
sendOptionsMessage :: (HasP2PEnv m, HasLogging m) => NodeId -> [NodeId] -> m ()
sendOptionsMessage _ [] = return ()
sendOptionsMessage sendingPeer (recievingPeer:peerList) = do
    _ <- LAsync.async (sendOptionsToPeer sendingPeer recievingPeer)
    sendOptionsMessage sendingPeer peerList

-- this function runs on each lightweight thread
-- two major functions
-- 1. Formulate and send options message
-- 2. Update the hashMap based oh the supported message returned
-- blocks while waiting for a response from the Other Peer
sendOptionsToPeer :: (HasP2PEnv m, HasLogging m) => NodeId -> NodeId -> m ()
sendOptionsToPeer sendingPeerNodeId recievingPeerNodeId = do
    let mMessage = Options {to = recievingPeerNodeId, from = sendingPeerNodeId}
    let byteStringMessage = serialise mMessage
    res1 <-
        Exception.try $ sendRequest recievingPeerNodeId Option byteStringMessage -- not exactly RPC, needs to be changed
    case res1 of
        Left (_ :: Exception.SomeException) -> throw SendOptionsFailedException
        Right returnMessage -> do
            let deserialiseCheck = deserialiseOrFail returnMessage
            case deserialiseCheck
                --TODO :: handle DeserialiseFailure exception
                  of
                Left _ -> return ()
                Right (deserialisedMessage :: MessageTypeRPC) ->
                    case deserialisedMessage of
                        Support _ fromPeer resourceList ->
                            Control.Monad.when (to mMessage == fromPeer) $
                            updateResourcePeers
                                (recievingPeerNodeId, resourceList)
                        _ ->
                            throw
                                (OptionsInvalidMessageType deserialisedMessage)

-- this wrapper will update the hashMap based on the supported message returned by the peer
updateResourcePeers ::
       (HasP2PEnv m, HasLogging m) => (NodeId, [ResourceId]) -> m ()
updateResourcePeers peerResourceTuple = do
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    let mNode = fst peerResourceTuple
    let listOfResources = snd peerResourceTuple
    _ <-
        liftIO $
        updateResourcePeersHelper
            mNode
            listOfResources
            archivedResourceToPeerMap
    return ()

-- adds the peer to the TQueue of each resource
-- lookup for the current resource in the HashMap
-- assumes that the resourceIDs are present in the HashMap
-- cannot add new currently because the serviceID is not available
updateResourcePeersHelper ::
       NodeId -> [ResourceId] -> ArchivedResourceToPeerMap -> IO Int
updateResourcePeersHelper _ [] _ = return 0
updateResourcePeersHelper mNodeId (currResource:listOfResources) archivedResourceToPeerMap = do
    let temp = HM.lookup currResource archivedResourceToPeerMap -- check for lookup returning Nothing
    case temp of
        Nothing ->
            updateResourcePeersHelper
                mNodeId
                listOfResources
                archivedResourceToPeerMap
        Just entry -> do
            let nodeListTVar = snd entry
            atomically
                (do nodeList <- readTVar nodeListTVar
                    let updatedList = nodeList ++ [mNodeId]
                    writeTVar nodeListTVar updatedList)
            tmp <-
                updateResourcePeersHelper
                    mNodeId
                    listOfResources
                    archivedResourceToPeerMap
            return $ 1 + tmp

-- | takes an options message and returns a supported message
optionsHandler :: (HasP2PEnv m) => P2PPayload -> m P2PPayload
optionsHandler payload = do
    let deserialiseCheck = deserialiseOrFail payload
    myId <- getSelfNodeId
    case deserialiseCheck of
        Left _
            -- the to = NodeId should be entered by P2P/Node end point
         -> do
            let errorMessage =
                    Response
                        { to = pack ""
                        , from = myId
                        , responseCode = DeserialiseError
                        }
            return $ serialise errorMessage
        Right (optionsMessage :: MessageTypeRPC) ->
            case optionsMessage of
                Options _ fromNodeId -> do
                    archivedResourceToPeerMapTvar <-
                        getArchivedResourceToPeerMapP2PEnv
                    archivedResourceToPeerMap <-
                        liftIO $ readTVarIO archivedResourceToPeerMapTvar
                    let resourceList = keys archivedResourceToPeerMap
                    let mMessage =
                            Support
                                { to = fromNodeId
                                , from = myId
                                , supportedResources = resourceList
                                }
                    let byteStringSupportMessage = serialise mMessage
                    return byteStringSupportMessage
                _ -> throw (OptionsHandlerInvalidMessageType optionsMessage)
