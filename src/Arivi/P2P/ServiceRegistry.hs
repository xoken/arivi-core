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
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception(try, SomeException)
import qualified Data.Map                   as Map
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
type SubscriptionTable = Map.Map ServiceCode PeerList
type NotifyTable = Map.Map ServiceCode PeerList
data AriviP2PInstance = AriviP2PInstance {  nodeId :: NodeId
                                            , ip:: String
                                            , port:: Int
                                            , outboundPeerQuota:: Float
                                            , maxConnectionAllowed:: Float}

-- fill hash with empty lists ServicePeerList
makeP2Pinstance ariviP2PInstanceTvar notifyTableTvar nodeId ip port
                outboundPeerQuota maxConnectionAllowed = do
    atomically( writeTVar ariviP2PInstanceTvar (AriviP2PInstance nodeId
                                                ip port outboundPeerQuota
                                                     maxConnectionAllowed) )
    let notifyTable = Map.fromList [ (BlockInventory,     []),
                                     (BlockSync,          []),
                                     (PendingTransaction, []) ]
    atomically( writeTVar notifyTableTvar notifyTable )
    forkIO (handleIncomingConnections outboundPeerQuota maxConnectionAllowed)
    return ()

_registerService ariviP2PInstanceTvar subscriptionTableTvar notifyTableTvar
                 serviceContextTvar serviceCode minPeerCount
                 transport peerType = do
    serCntxt <- atomically( readTVar serviceContextTvar )
    let context = (minPeerCount, peerType, transport)
    let newSerCntxt = Map.insert serviceCode context serCntxt
    atomically( writeTVar serviceContextTvar newSerCntxt )

    subscriptionTable <- atomically( readTVar subscriptionTableTvar )
    let hasServCode = Map.lookup serviceCode subscriptionTable
    case hasServCode of
        Nothing -> do
                   let newSubscriptionTable = Map.insert serviceCode [] subscriptionTable
                   atomically( writeTVar subscriptionTableTvar newSubscriptionTable )


    notifyTable <- atomically( readTVar notifyTableTvar )
    let hasServCode' = Map.lookup serviceCode notifyTable
    case hasServCode' of
        Nothing ->  do
                    let newNotifyTable = Map.insert serviceCode [] notifyTable
                    atomically( writeTVar subscriptionTableTvar newNotifyTable )

    ariviP2PInstance <- atomically( readTVar ariviP2PInstanceTvar )
    let outboundPeerQuota' = outboundPeerQuota ariviP2PInstance
    let maxConnectionAllowed' = maxConnectionAllowed ariviP2PInstance
    forkIO (outboundThread minPeerCount outboundPeerQuota' maxConnectionAllowed')


-- ======== Private functions =========
handleIncomingConnections outboundPeerQuota maxConnectionAllowed =  return ()

outboundThread minPeerCount outboundPeerQuota maxConnectionAllowed = return()

getPeerCount :: Either (ServiceCode,Context)
                       (ServiceCode,PeerList) -> PeerCount
getPeerCount (Left (_, (peerCount, _, _) )) = peerCount
getPeerCount (Right ( _ , peerList )) = length peerList

-- serviceNeedsPeers :: Monad m => ServicePeerList -> ServiceContext -> m Bool
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
