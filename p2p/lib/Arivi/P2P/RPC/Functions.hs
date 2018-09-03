{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Arivi.P2P.RPC.Functions
    ( registerResource
    , fetchResource
    -- -- not for Service Layer
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
import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Lens
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.HashMap.Strict                   as HM
import           Data.Maybe
import           Control.Applicative

-- register the resource and it's handler in the ResourceToPeerMap of RPC
registerResource ::
       (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => r
    -> ResourceHandler r msg
    -> ArchivedOrTransient
    -> m ()
registerResource resource resourceHandler resourceType = do
    archivedResourceToPeerMapTvar <- archived
    transientResourceToPeerMapTVar <- transient
    nodeIds <- liftIO $ newTVarIO [] -- create a new empty Tqueue for Peers
    case resourceType of
        Archived -> liftIO $ atomically $ modifyTVar' archivedResourceToPeerMapTvar (funcA nodeIds)
        Transient -> liftIO $ atomically $ modifyTVar' transientResourceToPeerMapTVar (funcT nodeIds)
    where
        funcA l hm = ArchivedResourceToPeerMap $ HM.insert resource (resourceHandler, l) (getArchivedMap hm)
        funcT l hm = TransientResourceToPeerMap $ HM.insert resource (resourceHandler, l) (getTransientMap hm)


-------------------- Functions for periodic updation of the hashmap ---------------------
-- creates a worker thread
-- thread should read the hashMap and should check if the number of peers for a resource is less than some number
-- if it is less should ask Kademlia for more nodes
-- send each peer and option message
-- the options message module will handle the sending of messages and updating of the HashMap based on the support message
updatePeerInResourceMap ::
       ( HasP2PEnv env m r msg
       )
    => r
    -> m ()
updatePeerInResourceMap r = do
    nId <- (^.networkConfig.nodeId) <$> ask
    archivedResourceToPeerMapTvar <- archived
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    let minimumNodes = 5 -- this value should be decided on and taken from the RPC environment
    _ <-
        LAsync.async
            (updatePeerInResourceMapHelper r
                 archivedResourceToPeerMap
                 minimumNodes
                 nId)
    return ()

updatePeerInResourceMapHelper ::
       ( HasP2PEnv env m r msg
       )
    => r
    -> ArchivedResourceToPeerMap r msg
    -> Int
    -> NodeId
    -> m ()
updatePeerInResourceMapHelper resource archivedResourceToPeerMap minimumNodes currNodeId =
    forever $ do
        let tempList = HM.toList (getArchivedMap archivedResourceToPeerMap)
        listOfLengths <- liftIO $ extractMin (fmap (snd . snd) tempList)
        let numberOfPeers = minimumNodes - listOfLengths
        if numberOfPeers > 0
            then do
                peerRandom <- Kademlia.getKRandomPeers 2
                res1 <- runExceptT $ Kademlia.getKClosestPeersByNodeid currNodeId 3
                -- TODO:: if getting closest peer fails what to do? currently getting random peers
                -- TODO:: handle certain exceptions specifically
                -- peersClose <-
                peersClose <- case res1 of
                    Left (_ :: AriviP2PException) ->
                        Kademlia.getKRandomPeers 3
                    Right peers -> return peers
                let peers = peerRandom ++ peersClose
                nodeIds <- addPeerFromKademlia peers
                sendOptionsMessage nodeIds (Options :: Options r)
                -- The thread delay is needed here put the updation thread to sleep for a given amount of time
                -- We want a periodic updation of the hashMap so, if the hashMap was updated the thread sleeps for a larger
                -- period of time (which needs to be decided after testing) but if the thread did not do any updation
                -- it sleeps for a smaller but fixed amount of time
                liftIO $ threadDelay (40 * 1000000) -- the timings need to be decided upon
            else liftIO $ threadDelay (30 * 1000000) -- in microseconds
        return ()


-- Extract the length of the minimum list from a list of TVar lists
extractMin ::[TVar [a]] -> IO Int
extractMin [] = return 0
extractMin l  = (fmap minimum <$> mapM (fmap length <$> readTVarIO)) l

fetchResource ::
       ( HasP2PEnv env m r msg
       )
    => RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
fetchResource payload@(RpcPayload resource _) = do
    nId <- (^. networkConfig . nodeId) <$> ask
    archivedResourceToPeerMapTvar <- archived
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    transientResourceToPeerMapTVar <- transient
    transientResourceToPeerMap <-
        liftIO $ readTVarIO transientResourceToPeerMapTVar
    let entryInArchivedResourceMap =
            HM.lookup resource (getArchivedMap archivedResourceToPeerMap)
    let entryInTransientResourceMap =
            HM.lookup resource (getTransientMap transientResourceToPeerMap)
    let entry = entryInArchivedResourceMap <|> entryInTransientResourceMap
    case entry of
        Nothing -> return (Left RPCResourceNotFoundException)
        Just entryMap -> do
            let nodeListTVar = snd entryMap
            nodeList <- liftIO $ atomically $ readTVar nodeListTVar
            liftIO $ print nodeList
            if null nodeList
                then return (Left RPCEmptyNodeListException)
                else sendResourceRequest nodeList payload

-- | Try fetching resource from a list of nodes. Return first successful response or return an error if didn't get a successfull response from any peer
sendResourceRequest :: ( HasP2PEnv env m r msg)
    => [NodeId]
    -> RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
sendResourceRequest [] _ = return (Left RPCResourceNotFoundException)
sendResourceRequest (currPeer:rest) msg = do
    res <- runExceptT $ issueRequest currPeer (RpcRequest msg)
    case res of
        Left _ -> sendResourceRequest rest msg
        Right (RpcResponse payload) ->
            case payload of
                resp@(RpcPayload _ _ ) -> return (Right resp)
                RpcError _ -> sendResourceRequest rest msg

-- | add the peers returned by Kademlia to the PeerDetails HashMap
addPeerFromKademlia ::
       (HasNodeEndpoint m, MonadIO m)
    => [Kademlia.Peer]
    -> m [NodeId]
addPeerFromKademlia peers = mapM (\peer -> do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    addPeerFromKademliaHelper peer nodeIdMapTVar) peers

addPeerFromKademliaHelper ::
       (MonadIO m)
    => Kademlia.Peer
    -> TVar NodeIdPeerMap
    -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar =
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
