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
import           Arivi.P2P.MessageHandler.HandlerTypes (Handle (..),
                                                        MessageType (..),
                                                        NodeIdPeerMap,
                                                        P2PPayload,
                                                        PeerDetails (..))
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialiseOrFail,
                                                        serialise)
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
import           Control.Applicative


class Resource i where
    resourceId :: i -> String

-- import           Debug.Trace
-- import           System.Random                         (randomRIO)
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
    let minimumNodes = 5 -- this value should be decided on and taken from the RPC environment
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
                    case res1
                        -- TODO:: if getting closest peer fails what to do? currently getting random peers
                        -- TODO:: handle certain exceptions specifically
                          of
                        Left (_ :: AriviP2PException) ->
                            Kademlia.getKRandomPeers 3
                            -- return KademliaGetClosePeersFailedException
                        Right peers -> return (fromRight peers)
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                sendOptionsMessage currNodeId nodeIds
                -- The thread delay is needed here put the updation thread to sleep for a given amount of time
                -- We want a periodic updation of the hashMap so, if the hashMap was updated the thread sleeps for a larger
                -- period of time (which needs to be decided after testing) but if the thread did not do any updation
                -- it sleeps for a smaller but fixed amount of time
                liftIO $ threadDelay (40 * 1000000) -- the timings need to be decided upon
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

getResource :: forall r . 
       (HasP2PEnv m, HasLogging m, Resource r, Serialise msg, Serialise r)
    => RpcPayload r msg
    -> m (RpcPayload r msg)
getResource (RpcPayload msg resource) = do
    mynodeid <- getSelfNodeId
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    transientResourceToPeerMap <-
        liftIO $ readTVarIO transientResourceToPeerMapTVar
    --resourceToPeerMap <- readTVarIO resourceToPeerMapTvar
    let entryInArchivedResourceMap =
            HM.lookup (resourceId resource) archivedResourceToPeerMap
    let entryInTransientResourceMap =
            HM.lookup (resourceId resource) transientResourceToPeerMap
    let entry = entryInArchivedResourceMap <|> entryInTransientResourceMap
    case entry of
        Nothing -> throw RPCResourceNotFoundException
        Just entryMap -> do
            let nodeListTVar = snd entryMap
            nodeList <- liftIO $ atomically $ readTVar nodeListTVar
            liftIO $ print nodeList
            if null nodeList
                then throw RPCEmptyNodeListException
                else sendResourceRequestToPeer
                         nodeListTVar
                         resource
                         mynodeid
                         msg

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
    res1 <- Exception.try $ issueRequest mNodeId RPC mMessage
    case res1 of
        Left (_ :: Exception.SomeException)
            -- removeNode mNodeId nodeList will be used when we take random nodes and not the head
         ->
            sendResourceRequestToPeer
                nodeListTVar
                resourceID
                mynodeid
                servicemessage -- should discard the peer
        Right payload -> do
            let deserialiseCheck = deserialiseOrFail payload
            case deserialiseCheck of
                Left _
                    -- the to = NodeId should be entered by P2P/Node end point
                 -> do
                    let errorMessage =
                            Response
                                { to = pack ""
                                , from = mNodeId
                                , responseCode = DeserialiseError
                                }
                    return $ ServiceMessage $ serialise errorMessage
                Right (inmessage :: MessageTypeRPC) ->
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
                                Error ->
                                    return $
                                    ServiceMessage $
                                    Lazy.fromStrict $ pack " error " -- need to define proper error handling maybe throw an exception
                                DeserialiseError -> do
                                    let errorMessage =
                                            Response
                                                { to = pack ""
                                                , from = mNodeId
                                                , responseCode =
                                                      DeserialiseError
                                                }
                                    return $
                                        ServiceMessage $ serialise errorMessage
         -- should check to and from
                        _ ->
                            return $
                            ServiceMessage $ Lazy.fromStrict $ pack " default"

-- will need the from NodeId to check the to and from
-- rpcHandler :: (HasP2PEnv m) => NodeId -> P2PPayload -> P2PPayload
rpcHandler :: (HasP2PEnv m) => P2PPayload -> m P2PPayload
rpcHandler incomingRequest = do
    let deserialiseCheck = deserialiseOrFail incomingRequest
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
        Right (incomingMessage :: MessageTypeRPC) ->
            case incomingMessage of
                RequestResource _ mNodeId resourceId requestServiceMessage -> do
                    archivedResourceToPeerMapTvar <-
                        getArchivedResourceToPeerMapP2PEnv
                    archivedResourceToPeerMap <-
                        liftIO $ readTVarIO archivedResourceToPeerMapTvar
                    transientResourceToPeerMapTVar <-
                        getTransientResourceToPeerMap
                    transientResourceToPeerMap <-
                        liftIO $ readTVarIO transientResourceToPeerMapTVar
                    let entryInArchivedResourceMap =
                            HM.lookup resourceId archivedResourceToPeerMap
                    let entryInTransientResourceMap =
                            HM.lookup resourceId transientResourceToPeerMap
                    let entry =
                            firstJust
                                entryInArchivedResourceMap
                                entryInTransientResourceMap
                    case entry of
                        Nothing -> throw RPCHandlerResourceNotFoundException
                        Just entryMap -> do
                            let resourceHandler = fst entryMap
                            let responseServiceMessage =
                                    resourceHandler requestServiceMessage
                            let replyMessage =
                                    ReplyResource
                                        { to = mNodeId
                                        , from = myId
                                        , rid = resourceId
                                        , serviceMessage =
                                              responseServiceMessage
                                        }
                            let rpcResponse = serialise replyMessage
                            return rpcResponse
                    -- currently catching everything and returning an empty byte string in future need to define proper error messages
                _ -> throw (RPCInvalidMessageType incomingMessage)

-- | add the peers returned by Kademlia to the PeerDetails HashMap
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
                let newIp = Just (KademliaTypes.nodeIp kadNodeEndPoint)
                let newUdpPort = Just (KademliaTypes.udpPort kadNodeEndPoint)
                let newTcpPort = Just (KademliaTypes.tcpPort kadNodeEndPoint)
                do case mapEntry of
                       Nothing -> do
                           let newDetails =
                                   PeerDetails
                                       { nodeId = mNodeId
                                       , rep = Nothing
                                       , ip = newIp
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
                       Just value -> do
                           oldPeerDetails <- readTVar value
                           let newDetails =
                                   oldPeerDetails
                                       { ip = newIp
                                       , udpPort = newUdpPort
                                       , tcpPort = newTcpPort
                                       }
                           writeTVar (fromJust mapEntry) newDetails
                   return mNodeId)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust entryInArchivedResourceMap entryInTransientResourceMap
    | isJust entryInArchivedResourceMap = entryInArchivedResourceMap
    | isJust entryInTransientResourceMap = entryInTransientResourceMap
    | otherwise = Nothing
-- removeNode :: NodeId -> [NodeId] -> [NodeId]
-- removeNode _ []                 = []
-- removeNode x (y:ys) | x == y    = removeItem x ys
--                     | otherwise = y : removeItem x ys
