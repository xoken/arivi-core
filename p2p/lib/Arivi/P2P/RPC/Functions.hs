{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Arivi.P2P.RPC.Functions
    ( registerResource
    , fetchResource
    -- -- not for Service Layer
    , rpcHandler
    , updatePeerInResourceMap
    , addPeerFromKademlia
    ) where

import           Arivi.P2P.Types
import           Arivi.P2P.Exception
import qualified Arivi.P2P.Kademlia.Kbucket            as Kademlia (Peer (..),
                                                                    getKClosestPeersByNodeid,
                                                                    getKRandomPeers)
import qualified Arivi.P2P.Kademlia.Types              as KademliaTypes (NodeEndPoint (..))
import           Arivi.P2P.MessageHandler.HandlerTypes (NodeIdPeerMap,
                                                        defaultPeerDetails,
                                                        HasNetworkConfig(..))
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.RPC.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (try)
import           Control.Lens
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Either.Unwrap
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
import           Control.Applicative

-- import           Debug.Trace
-- import           System.Random                         (randomRIO)
-- register the resource and it's handler in the ResourceToPeerMap of RPC
registerResource ::
       (HasP2PEnv m)
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
updatePeerInResourceMap :: (MonadReader env m, HasNetworkConfig env NetworkConfig, HasP2PEnv m, HasLogging m) => m ()
updatePeerInResourceMap = do
    nId <- (^.networkConfig.nodeId) <$> ask
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    let minimumNodes = 5 -- this value should be decided on and taken from the RPC environment
    _ <-
        LAsync.async
            (updatePeerInResourceMapHelper
                 archivedResourceToPeerMap
                 minimumNodes
                 nId)
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
                -- TODO:: if getting closest peer fails what to do? currently getting random peers
                -- TODO:: handle certain exceptions specifically
                peersClose <-
                    case res1 of
                        Left (_ :: AriviP2PException) ->
                            Kademlia.getKRandomPeers 3
                        Right peers -> return (fromRight peers)
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                sendOptionsMessage nodeIds
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

fetchResource ::
       ( MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasP2PEnv m
       , HasLogging m
       , Resource r
       , Serialise msg
       )
    => RpcPayload r msg
    -> m (RpcPayload r msg)
fetchResource payload@(RpcPayload resource _) = do
    nId <- (^.networkConfig.nodeId) <$> ask
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    transientResourceToPeerMapTVar <- getTransientResourceToPeerMap
    transientResourceToPeerMap <-
        liftIO $ readTVarIO transientResourceToPeerMapTVar
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
                         nId
                         payload

sendResourceRequestToPeer ::
       (MonadReader env m, HasNetworkConfig env NetworkConfig, HasP2PEnv m, HasLogging m, Resource r, Serialise msg)
    => TVar [NodeId]
    -> NodeId
    -> RpcPayload r msg
    -> m (RpcPayload r msg)
sendResourceRequestToPeer nodeListTVar nId msg = do
    nodeList <- liftIO $ readTVarIO nodeListTVar
    let mNodeId = head nodeList
    -- let requestMessage =
    --         RequestResource
    --         { to = mNodeId
    --         , from = mynodeid
    --         , rid = resourceID -- add RID
    --         , serviceMessage = servicemessage
    --         }
    -- let mMessage = serialise requestMessage
    res1 <- runExceptT $ issueRequest mNodeId (RpcRequest msg)
    -- removeNode mNodeId nodeList will be used when we take random nodes and not the head
    case res1 of
        Left _ -> sendResourceRequestToPeer
                      nodeListTVar
                      nId
                      msg -- should discard the peer
        Right (RpcResponse resp) -> return resp
         -- case msg of
         --                ReplyResource toNodeId fromNodeId resID _ ->
         --                    if (mynodeid == toNodeId && mNodeId == fromNodeId) &&
         --                       resourceID == resID
         --                        then liftIO $ return (resp)
         --                        else sendResourceRequestToPeer
         --                                 nodeListTVar
         --                                 resourceID
         --                                 mynodeid
         --                                 servicemessage
         --                Response _ _ responseCode' ->
         --                    case responseCode' of
         --                        Busy ->
         --                            sendResourceRequestToPeer
         --                                nodeListTVar
         --                                resourceID
         --                                mynodeid
         --                                servicemessage
         --                        Error ->
         --                            return $ Lazy.fromStrict $ pack " error " -- need to define proper error handling maybe throw an exception
         --                        DeserialiseError ->
         --                            return $
         --                            Lazy.fromStrict $ pack " Deserialise error "
         -- -- should check to and from
         --                _ -> return $ Lazy.fromStrict $ pack " default"

-- will need the from NodeId to check the to and from
-- rpcHandler :: (HasP2PEnv m) => NodeId -> P2PPayload -> P2PPayload
rpcHandler ::
       ( HasP2PEnv m
       , Resource r
       , Serialise msg
       )
    => RpcPayload r msg
    -> m (RpcPayload r msg)
rpcHandler req = do
    archivedResourceMap <- getArchivedResourceToPeerMapP2PEnv >>= (liftIO . readTVarIO)
    transientResourceMap <- getTransientResourceToPeerMap >>= (liftIO . readTVarIO)
    let entry =  transientResourceMap ^. at (resourceId req)
             <|> archivedResourceMap ^. at (resourceId req)
    case entry of
        Nothing -> throw RPCHandlerResourceNotFoundException
        Just entryMap -> do
            let ResourceHandler resourceHandler = fst entryMap
            return (resourceHandler req)

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
       (HasP2PEnv m)
    => Kademlia.Peer
    -> TVar NodeIdPeerMap
    -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar = do
    liftIO $
        atomically
            (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
                let _nodeId = fst $ Kademlia.getPeer peerFromKademlia
                    kadNodeEndPoint = snd $ Kademlia.getPeer peerFromKademlia
                    mapEntry = HM.lookup _nodeId nodeIdPeerMap
                    _ip = KademliaTypes.nodeIp kadNodeEndPoint
                    _udpPort = KademliaTypes.udpPort kadNodeEndPoint
                    _tcpPort = KademliaTypes.tcpPort kadNodeEndPoint
                case mapEntry of
                    Nothing -> do
                        defaultPeer <-
                            (& networkConfig .~ NetworkConfig {..}) <$>
                            defaultPeerDetails
                        newPeer <- newTVar defaultPeer
                        let newHashMap = HM.insert _nodeId newPeer nodeIdPeerMap
                        writeTVar nodeIdPeerMapTVar newHashMap
                    Just value -> do
                        oldPeerDetails <- readTVar value
                        let newDetails =
                                oldPeerDetails & networkConfig .~
                                NetworkConfig {..}
                        writeTVar (fromJust mapEntry) newDetails
                return _nodeId)
