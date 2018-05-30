-- |
-- Module      :  Arivi.P2P.ServiceRegistry
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- ServiceRegistry is part of Arivi P2P layer, It keeps track of ServiceContext
-- and ConnectionCommand
module Arivi.P2P.ServiceRegistry
(
    --   ConnectionCommand(..)
    -- , ContextId
    --   ServiceContext
    -- , genUniqueSessionId
    -- , _registerService
) where

import           Arivi.Network.Connection     (Connection, ConnectionId)
import           Arivi.Network.Types          (TransportType (..))
import           Arivi.P2P.PubSub             (Topic)
import           Arivi.P2P.Types
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception(try, SomeException)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.List                   as List
import           Data.HashTable.IO
import           Data.Maybe                   (Maybe)
import           Data.UUID                    (UUID)
import           Data.UUID.V1                 (nextUUID)
import           Network(connectTo, PortID(..), PortNumber(..))
import           System.IO
import           Arivi.Kademlia.Query
import           Arivi.Kademlia.Types

data NodeType = FullNode | HalfNode deriving(Show)
data ServiceCode = BlockInventory | BlockSync | PendingTransaction
                   deriving(Eq,Ord)

type IP = String
type Port = Int
type Timeout = Float
type PeerList = [ (NodeId,IP,Port,Timeout) ]
type PeerCount = Int
type Context = (PeerCount, NodeType, TransportType)

type ServiceContext  = Map.Map ServiceCode Context
type SubscriptionMap = Map.Map ServiceCode PeerList
type WatchersMap = Map.Map ServiceCode PeerList
data AriviP2PInstance = AriviP2PInstance { nodeId :: NodeId
                                         , ip:: String
                                         , port:: Int
                                         , outboundPeerQuota:: Float
                                         , maxConnectionAllowed:: Float}

-- fill hash with empty lists ServicePeerList
makeP2Pinstance ariviP2PInstanceTvar watchersMapTvar subscriptionMapTvar nodeId ip port
                outboundPeerQuota maxConnectionAllowed = do
    atomically( writeTVar ariviP2PInstanceTvar (AriviP2PInstance nodeId
                                                ip port outboundPeerQuota
                                                     maxConnectionAllowed) )
    let watchersMap = Map.fromList [ (BlockInventory,     []),
                                     (BlockSync,          []),
                                     (PendingTransaction, []) ]
    let subscriptionMap = Map.fromList [ (BlockInventory,     []),
                                     (BlockSync,          []),
                                     (PendingTransaction, []) ]
    atomically( writeTVar watchersMapTvar watchersMap )
    atomically( writeTVar subscriptionMapTvar subscriptionMap )
    forkIO (handleIncomingConnections subscriptionMapTvar watchersMapTvar
                            outboundPeerQuota maxConnectionAllowed)
    return ()

_registerService ariviP2PInstanceTvar subscriptionMapTvar watchersMapTvar
                 serviceContextTvar serviceCode minPeerCount
                 transport peerType = do
    serCntxt <- atomically( readTVar serviceContextTvar )
    let context = (minPeerCount, peerType, transport)
    let newSerCntxt = Map.insert serviceCode context serCntxt
    atomically( writeTVar serviceContextTvar newSerCntxt )

    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    let hasServCode = Map.lookup serviceCode subscriptionMap
    case hasServCode of
        Nothing -> do
                   let newSubscriptionMap = Map.insert serviceCode [] subscriptionMap
                   atomically( writeTVar subscriptionMapTvar newSubscriptionMap )


    watchersMap <- atomically( readTVar watchersMapTvar )
    let hasServCode' = Map.lookup serviceCode watchersMap
    case hasServCode' of
        Nothing ->  do
                    let newWatchersMap = Map.insert serviceCode [] watchersMap
                    atomically( writeTVar subscriptionMapTvar newWatchersMap )

    ariviP2PInstance <- atomically( readTVar ariviP2PInstanceTvar )
    let outboundPeerQuota' = outboundPeerQuota ariviP2PInstance
    let maxConnectionAllowed' = maxConnectionAllowed ariviP2PInstance
    forkIO (outboundThread subscriptionMapTvar watchersMapTvar
                           minPeerCount maxConnectionAllowed')

-- ======== Private functions =========
handleIncomingConnections subscriptionMapTvar watchersMapTvar outboundPeerQuota
                          maxConnectionAllowed = forever $ do
    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    watchersMap <- atomically( readTVar watchersMapTvar )
    let uniqueLen = length . Set.fromList . (List.intercalate []) . Map.elems
    let subscriptionLen = uniqueLen subscriptionMap
    let totalCount = subscriptionLen + uniqueLen watchersMap
    return ()
    -- if subscriptionLen/totalCount > outboundPeerQuota &&
    --    totalCount < maxConnectionAllowed then
    --     --accept incoming connection
    -- else
    --     --wait for some time
    --     updateWatchersMapTime watchersMap

outboundThread subscriptionMapTvar watchersMapTvar minPeerCount maxConnectionAllowed = do
    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    watchersMap <- atomically( readTVar watchersMapTvar )
    let uniqueLen = length . Set.fromList . (List.intercalate []) . Map.elems
    let totalCount = uniqueLen subscriptionMap + uniqueLen watchersMap
    return ()
    -- if totalCount < maxConnectionAllowed && totalCount<minPeerCount then
    --         -- subscribe ( nodeid, topic)
-- ================================================


getPeerCount :: Either (ServiceCode,Context)
                       (ServiceCode,PeerList) -> PeerCount
getPeerCount (Left (_, (peerCount, _, _) )) = peerCount
getPeerCount (Right ( _ , peerList )) = length peerList

totalCount subscriptionMapTvar watchersMapTvar = do
    watchersMap <- atomically( readTVar watchersMapTvar )
    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    let watchersList = Map.toList watchersMap
    let subscriptionList = Map.toList subscriptionMap
    return (counterPeers watchersList + counterPeers subscriptionList) where
        counterPeers table = case table of [] -> 0
                                           (x:xs) -> getPeerCount (Right x) +
                                                    counterPeers (tail table)

-- serviceNeedsPeers servicePeerList serviceContext =
--     serviceNeedsPeers1 (Map.toList servicePeerList) (Map.toList serviceContext)
--     where
--         serviceNeedsPeers1 servicePeerList serviceContext = do
--             case length serviceContext of 0 -> return False
--             let requiredPeers  = getPeerCount .Left  .head
--             let peerListLength = getPeerCount .Right .head
--             if peerListLength servicePeerList < requiredPeers serviceContext
--             then return True
--             else serviceNeedsPeers1 (tail servicePeerList) (tail serviceContext)

-- findMaxPeerfromMap servicePeerList = findMaxPeerService (Map.toList servicePeerList)
-- findMaxPeerService:: [ ( ServiceCode,PeerList ) ] -> Int
-- findMaxPeerService [x] = getPeerCount (Right x)
-- findMaxPeerService (x:xs)
--     | findMaxPeerService xs > getPeerCount (Right x) = findMaxPeerService xs
--     | otherwise = getPeerCount (Right x)

-- negotiateGetPeers servicePeerListTvar serviceContextTvar peerCount = do
--     case peerCount of 0 -> return ()
--     (ip,port,nodeId) <- getAvailablePeer peerType transport
--     -- FSM NEGOTIATES AND UPDATES ServicePeerList
--     --handle ServicePeerList connection Idle InitServiceNegotiationEvent
--     negotiateGetPeers servicePeerListTvar transport peerType (peerCount-1)

-- -- loop negotiateGetPeers servicePeerList serviceContext
-- --      transport peerType peerCount= do
-- --     negotiateGetPeers servicePeerList transport peerType peerCount
-- --     doServiceNeedPeers <- serviceNeedsPeers servicePeerList serviceContext
-- --     case doServiceNeedPeers of
-- --         True -> loop negotiateGetPeers servicePeerList transport
-- --                      peerType peerCount

-- peerMaintenanceThread servicePeerListTvar serviceContextTvar peerType = do
--     servicePeerList <- atomically( readTVar servicePeerListTvar )
--     serviceContext  <- atomically( readTVar serviceContextTvar )
--     case (Map.size serviceContext)>0 of True -> do
--         let maxPeerSer = findMaxPeerfromMap servicePeerList
--         negotiateGetPeers servicePeerListTvar serviceContextTvar maxPeerSer
--         forever $ do
--             let minPeer = 4
--             loop negotiateGetPeers servicePeerListTvar transport peerType minPeer
--         -- where loop negotiateGetPeers servicePeerList transport peerType minPeer= do
--         --             negotiateGetPeers servicePeerList transport peerType minPeer
--         --             case serviceNeedsPeers servicePeerList
--         --                                    serviceContext of True -> loop negotiateGetPeers
--         --                                                                   servicePeerList
--         --                                                                   transport
--         --                                                                   peerType
--         --                                                                   minPeer
-- --         --wait for timeout

-- -- -- data ServiceRegistry = ServiceRegistry {
-- -- --                 hashTable     :: CuckooHashTable
-- -- --               , contextId     :: ContextId
-- -- --               , serviceContex :: ServiceContext }
-- -- -- ConnectionCommand keeps track of Connection Shared Variable
-- -- -- data ConnectionCommand = ConnectionCommand {
-- --     -- connectionId   :: ConnectionId   -- ^ Unique Connection Identifier
-- --     -- ,connectionMVar :: MVar Connection-- ^ Shared Variable  for connection }
-- -- --      -> CuckooHashTable ServiceCode [Topic]
-- -- --      -> IO (CuckooHashTable ServiceCode [Topic])


-- -- sampleServiceContext :: ServiceContext
-- -- sampleServiceContext = Map.fromList [ (213, (2, FullNode, UDP) ),
-- --                                       (123, (3, HalfNode, TCP) ) ]
