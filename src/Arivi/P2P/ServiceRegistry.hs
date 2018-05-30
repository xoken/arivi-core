{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import           Codec.Serialise
import           GHC.Generics


data NodeType = FullNode | HalfNode deriving(Show)
-- Added Show in deriving
data ServiceCode = BlockInventory | BlockSync | PendingTransaction deriving(Eq,Ord,Show,Generic)

type IP = String
type Port = Int
type Timeout = Float
type PeerList = [ (NodeId,IP,Port,Timeout) ]
type PeerCount = Int
type Context = (PeerCount, NodeType, TransportType)

type ServiceContext  = Map.Map ServiceCode Context
type SubscriptionTable = Map.Map ServiceCode PeerList
type NotifyTable = Map.Map ServiceCode PeerList


-- fill hash with empty lists ServicePeerList
-- launch peer maintenance thread
makeP2Pinstance servicePeerListTvar serviceContextTvar nodeId ip port = do
    let servPeerList = Map.fromList [ (BlockInventory,     []),
                                      (BlockSync,          []),
                                      (PendingTransaction, []) ]
    atomically( writeTVar servicePeerListTvar servPeerList )

    forkIO (peerMaintenanceThread servicePeerListTvar serviceContextTvar)
    return ()

_registerService serviceContextTvar ariviInstance serviceCode
                 transport peerType peerCount= do
                    serCntxt <- atomically( readTVar serviceContextTvar )
                    let context = (peerCount, peerType, transport)
                    let newSerCntxt = Map.insert serviceCode context serCntxt
                    atomically( writeTVar serviceContextTvar newSerCntxt )


-- -- ======== Private functions =========
getPeerCount :: Either (ServiceCode,Context)
                       (ServiceCode,PeerList) -> PeerCount
getPeerCount (Left (_, (peerCount, _, _) )) = peerCount
getPeerCount (Right ( _ , peerList )) = length peerList

serviceNeedsPeers :: Monad m => ServicePeerList -> ServiceContext -> m Bool
serviceNeedsPeers servicePeerList serviceContext =
    serviceNeedsPeers1 (Map.toList servicePeerList) (Map.toList serviceContext)
    where
        serviceNeedsPeers1 servicePeerList serviceContext = do
            case length serviceContext of 0 -> return False
            let requiredPeers  = getPeerCount .Left  .head
            let peerListLength = getPeerCount .Right .head
            if peerListLength servicePeerList < requiredPeers serviceContext
            then return True
            else serviceNeedsPeers1 (tail servicePeerList) (tail serviceContext)

findMaxPeerfromMap servicePeerList = findMaxPeerService (Map.toList servicePeerList)
findMaxPeerService:: [ ( ServiceCode,PeerList ) ] -> Int
findMaxPeerService [x] = getPeerCount (Right x)
findMaxPeerService (x:xs)
    | findMaxPeerService xs > getPeerCount (Right x) = findMaxPeerService xs
    | otherwise = getPeerCount (Right x)

negotiateGetPeers servicePeerListTvar serviceContextTvar peerCount = do
    case peerCount of 0 -> return ()
    (ip,port,nodeId) <- getAvailablePeer peerType transport
    -- FSM NEGOTIATES AND UPDATES ServicePeerList
    --handle ServicePeerList connection Idle InitServiceNegotiationEvent
    negotiateGetPeers servicePeerListTvar transport peerType (peerCount-1)

-- loop negotiateGetPeers servicePeerList serviceContext
--      transport peerType peerCount= do
--     negotiateGetPeers servicePeerList transport peerType peerCount
--     doServiceNeedPeers <- serviceNeedsPeers servicePeerList serviceContext
--     case doServiceNeedPeers of
--         True -> loop negotiateGetPeers servicePeerList transport
--                      peerType peerCount

peerMaintenanceThread servicePeerListTvar serviceContextTvar peerType = do
    servicePeerList <- atomically( readTVar servicePeerListTvar )
    serviceContext  <- atomically( readTVar serviceContextTvar )
    case (Map.size serviceContext)>0 of True -> do
        let maxPeerSer = findMaxPeerfromMap servicePeerList
        negotiateGetPeers servicePeerListTvar serviceContextTvar maxPeerSer
        forever $ do
            let minPeer = 4
            loop negotiateGetPeers servicePeerListTvar transport peerType minPeer
        -- where loop negotiateGetPeers servicePeerList transport peerType minPeer= do
        --             negotiateGetPeers servicePeerList transport peerType minPeer
        --             case serviceNeedsPeers servicePeerList
        --                                    serviceContext of True -> loop negotiateGetPeers
        --                                                                   servicePeerList
        --                                                                   transport
        --                                                                   peerType
        --                                                                   minPeer
--         --wait for timeout

-- -- data ServiceRegistry = ServiceRegistry {
-- --                 hashTable     :: CuckooHashTable
-- --               , contextId     :: ContextId
-- --               , serviceContex :: ServiceContext }
-- -- ConnectionCommand keeps track of Connection Shared Variable
-- -- data ConnectionCommand = ConnectionCommand {
--     -- connectionId   :: ConnectionId   -- ^ Unique Connection Identifier
--     -- ,connectionMVar :: MVar Connection-- ^ Shared Variable  for connection }
-- --      -> CuckooHashTable ServiceCode [Topic]
-- --      -> IO (CuckooHashTable ServiceCode [Topic])


-- sampleServiceContext :: ServiceContext
-- sampleServiceContext = Map.fromList [ (213, (2, FullNode, UDP) ),
--                                       (123, (3, HalfNode, TCP) ) ]

import Arivi.Network.Types (ConnectionId, PersonalityType(..), TransportType(..))
import Arivi.Network.instance (openConnection, sendMessage)
-- import Arivi.kademila. (Get_Peer_List)

data SubscribeMessage = SubscribeMessage {
    neededServices :: [ServiceCode], 
    avaiableServices :: [ServiceCode] 
    } deriving (Eq,Show)

-- SubscribeMessage 
instance Serialise SubscribeMessage

SubscribeMessage :: ConnectionId -> [a]-> IO
SubscribeMessage ConnectionId ServicesNeeded= do
    -- searialise the list and send 
    
    -- serialise servicesList
    
    -- avaiableServices = Map.keys leServiceContext
    -- let subMsg = SubscribeMessage {neededServices = [BlockInventory,PendingTransaction], avaiableServices = [BlockSync] }

    -- convert subMsg to Serialize

    -- Network Layer API call
    sendMessage ConnectionId BString


-- Thread which sends subscribe request messages to peers taken from kademila
addSubscribePeer :: IO()
addSubscribePeer = do

    neededServices = getListOfServicesNeeded ServiceContext SubscriptionTable NotifyTable

    if neededServices == []    
        then do
            -- wait for some constant seconds
            wait(5)
            addSubscribePeer
    else do 
    
        -- Ask for one peer from kademila
        -- kademila function
        peerlist <- Get_Peer_List(1)

        -- take the first peer data and open it
        (IP,PORT,NodeId) <- peerlist!!1

        -- network layer function
        connId <- openConnection(IP, PORT, TCP, NodeId, INITIATOR)

        SubscribeMessage(connId, ServicesNeeded)

        addSubscribePeer

    where
        neededServices = getListOfServicesNeeded ServiceContext SubscriptionTable NotifyTable


-- subscription (i.e is outbound thread ) is only called when services are need and that is when
-- number of peer connections drops less then minPeerCount
-- and not when when raito is violated

-- incoming is only accepted when before hand checked that if addded raito is not violated


-- Finds out if service needs peer based on minimun peer count taking ServiceCode as input
doesServiceNeedPeers :: ServiceCode -> ServiceContext -> SubscriptionTable -> NotifyTable -> [ServiceCode]
doesServiceNeedPeers serviceCode serviceContext subscriptionTable notifyTable= do
            
    let serContext = fromJust $ Map.lookup serviceCode serviceContext
    let subpeerlist = fromJust $ Map.lookup serviceCode subscriptionTable
    let notifypeerlist = fromJust $ Map.lookup serviceCode notifyTable
        
    let totalconnectioncount = toInteger ((getPeerCount (Right (serviceCode,subpeerlist)) ) + (getPeerCount (Right (serviceCode,notifypeerlist)) ))
        
    if (totalconnectioncount < (toInteger (getPeerCount (Left (serviceCode,serContext)))) )
        then [serviceCode]
    else
        []
    
-- Gets List of services which needs peers
getListOfServicesNeeded :: ServiceContext -> SubscriptionTable -> NotifyTable -> [ServiceCode]
getListOfServicesNeeded serContext subTable notifyTable = do
    let keysList = Map.keys serContext
    goThroughEachService keysList [] serContext subTable notifyTable



-- Loop through each service finding whether it needs service
goThroughEachService :: [ServiceCode]-> [ServiceCode] -> ServiceContext -> SubscriptionTable -> NotifyTable -> [ServiceCode]
goThroughEachService [] result serContext subTable notifyTable = result
goThroughEachService (ser:serList) result serContext subTable notifyTable =
    goThroughEachService serList (result ++ val) serContext subTable notifyTable
    where
        val = doesServiceNeedPeers ser serContext subTable notifyTable


