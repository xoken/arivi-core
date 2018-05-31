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
    -- , registerTopic
) where

import           Arivi.Network.Connection     (Connection)
import           Arivi.Network.Types          (TransportType (..))
import           Arivi.P2P.PubSub             (Topic)
import           Arivi.P2P.Types
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception            (try, SomeException)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.List                    as List
import           Data.HashTable.IO
import           Data.Maybe                   (Maybe)
import           Data.UUID                    (UUID)
import           Data.UUID.V1                 (nextUUID)
import           Network                      (connectTo, PortID(..), PortNumber(..))
import           System.IO
import           Arivi.Kademlia.Query
import           Arivi.Kademlia.Types
import           Codec.Serialise
import           GHC.Generics
import           Data.ByteString.Char8        (pack)
import           Data.Maybe                   (fromJust)

data NodeType = FullNode | HalfNode deriving(Show)

data ServiceCode = BlockInventory | BlockSync | PendingTransaction 
					deriving(Eq,Ord,Show,Generic)


data TopicCode = Latest_block_header | Latest_block
                    deriving(Eq,Ord)

type IP = String
type Port = Int
type ExpiryTime = Float
type PeerList = [ (NodeId,IP,Port,ExpiryTime) ]
type MinPeerCountPerTopic = Int
type Context = (MinPeerCountPerTopic, NodeType, TransportType)

-- type ServiceContext  = Map.Map ServiceCode Context
-- type TopicToService  = Map.Map ServiceCode Context
-- type SubscriptionMap = Map.Map ServiceCode PeerList
-- type WatchersMap = Map.Map ServiceCode PeerList

type TopicContext  = Map.Map TopicCode Context
type TopicToService  = Map.Map TopicCode ServiceCode
type SubscriptionMap = Map.Map TopicCode PeerList
type WatchersMap = Map.Map TopicCode PeerList

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
    -- thread for asking kademlia for peer and ask for topics available
    -- and modify GLOBAL MATRIX which maintains the ranking of peers 
    -- which provide more topics.
    return ()

registerTopicModSerCntxtTvar :: TopicCode -> MinPeerCountPerTopic -> NodeType -> TransportType-> TopicContext -> TopicContext
registerTopicModSerCntxtTvar topicCode minPeerCountPerTopic peerType transport serCntxt = newSerCntxt
    where   context = (minPeerCountPerTopic, peerType, transport)
            newSerCntxt = Map.insert topicCode context serCntxt

registerTopicSubMapOrWatchMapTvar topicCode subscriptionSubOrWatchMap = do
    let hasServCode = Map.lookup topicCode subscriptionSubOrWatchMap
    case hasServCode of
        Nothing ->  Map.insert topicCode [] subscriptionSubOrWatchMap
        _ -> subscriptionSubOrWatchMap


registerTopic ariviP2PInstanceTvar subscriptionMapTvar watchersMapTvar
                 topicContextTvar topicCode  minPeerCountPerTopic
                 transport peerType = do
    
    --let context = (minPeerCountPerTopic, peerType, transport)
    --let newSerCntxt = Map.insert serviceCode context serCntxt
    --atomically( writeTVar serviceContextTvar newSerCntxt )

    atomically( modifyTVar topicContextTvar (registerTopicModSerCntxtTvar topicCode minPeerCountPerTopic peerType transport))


    -- subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    -- let hasServCode = Map.lookup serviceCode subscriptionMap
    -- case hasServCode of
    --     Nothing -> do
    --                let newSubscriptionMap = Map.insert serviceCode [] subscriptionMap
    --                atomically( writeTVar subscriptionMapTvar newSubscriptionMap )
    atomically( modifyTVar subscriptionMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))
    atomically( modifyTVar watchersMapTvar (registerTopicSubMapOrWatchMapTvar topicCode))

    -- watchersMap <- atomically( readTVar watchersMapTvar )
    -- let hasServCode' = Map.lookup serviceCode watchersMap
    -- case hasServCode' of
    --     Nothing ->  do
    --                 let newWatchersMap = Map.insert serviceCode [] watchersMap
    --                 atomically( writeTVar subscriptionMapTvar newWatchersMap )

    ariviP2PInstance <- atomically( readTVar ariviP2PInstanceTvar )
    let outboundPeerQuota' = outboundPeerQuota ariviP2PInstance
    let maxConnectionAllowed' = maxConnectionAllowed ariviP2PInstance

    topicContext <- atomically( readTVar topicContextTvar )
    forkIO (outboundThread topicCode topicContext subscriptionMapTvar watchersMapTvar minPeerCountPerTopic maxConnectionAllowed')

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

-- outboundThread subscriptionMapTvar watchersMapTvar minPeerCountPerTopic maxConnectionAllowed = do
--     subscriptionMap <- atomically( readTVar subscriptionMapTvar )
--     watchersMap <- atomically( readTVar watchersMapTvar )
--     let uniqueLen = length . Set.fromList . (List.intercalate []) . Map.elems
--     let totalCount = uniqueLen subscriptionMap + uniqueLen watchersMap
--     return ()
--     -- if totalCount < maxConnectionAllowed && totalCount<minPeerCountPerTopic then
--     --         -- subscribe ( nodeid, topic)
-- -- ================================================


getPeerCount :: Either (TopicCode,Context)
                       (TopicCode,PeerList) -> MinPeerCountPerTopic
getPeerCount (Left (_, (peerCount, _, _) )) = peerCount
getPeerCount (Right ( _ , peerList )) = length peerList

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
-- findMaxPeerService:: [ ( serviceCode,PeerList ) ] -> Int
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
-- -- --      -> CuckooHashTable serviceCode [Topic]
-- -- --      -> IO (CuckooHashTable serviceCode [Topic])


-- -- sampleServiceContext :: ServiceContext
-- -- sampleServiceContext = Map.fromList [ (213, (2, FullNode, UDP) ),
-- --                                       (123, (3, HalfNode, TCP) ) ]



-- changes
-- changes  important one: service -> topic


-- Finds out if service needs peer based on minimun peer count taking serviceCode as input
doesServiceNeedPeers :: TopicCode -> TopicContext -> SubscriptionMap -> WatchersMap -> Bool
doesServiceNeedPeers topicCode topicContext subscriptionMap watchersMap = do
            
    let serContext = fromJust $ Map.lookup topicCode topicContext
    let subpeerlist = fromJust $ Map.lookup topicCode subscriptionMap
    let notifypeerlist = fromJust $ Map.lookup topicCode watchersMap
        
    let totalconnectioncount = toInteger ((getPeerCount (Right (topicCode,subpeerlist)) ) + 
                                          (getPeerCount (Right (topicCode,notifypeerlist)) ))
        
    if (totalconnectioncount < (toInteger (getPeerCount (Left (topicCode,serContext)))) )
        then True
    else
        False

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

-- -- changes added topic code
outboundThread topicCode topicContext subscriptionMapTvar watchersMapTvar minPeerCount maxConnectionAllowed = do

    subscriptionMap <- atomically( readTVar subscriptionMapTvar )
    watchersMap <- atomically( readTVar watchersMapTvar )

    -- Changes from interclate to flatten

    -- Map.elems gets a list of all peerList of all the topicscode
    -- flatten convert list of list to just a single list
    -- set.formlist removes all the repeated elemets (i.r gives unique nodes)
    let uniqueLen map = length $ Set.fromList $ flatten $ Map.elems map 
    let totalCount = uniqueLen subscriptionMap + uniqueLen watchersMap
    
    

    if totalCount < maxConnectionAllowed && (doesServiceNeedPeers topicCode topicContext subscriptionMap watchersMap)
        then do
        let connectionId = getnCheckConnection

        subscribeMessage connectionId topicCode
        outboundThread topicCode topicContext subscriptionMapTvar  watchersMapTvar minPeerCount maxConnectionAllowed
    else
        outboundThread topicCode topicContext subscriptionMapTvar  watchersMapTvar minPeerCount maxConnectionAllowed 


getnCheckConnection = pack "9823593847"
                    -- check if connection already exits
                    -- if connection does not exits 
                    -- network layer  api -> openConnection

subscribeMessage :: ConnectionId -> TopicCode -> IO ()
subscribeMessage connectionId topicNeeded= do 
    -- serialise topicNeeded -> BString
    -- Network Layer API call
--   sendMessage ConnectionId BString
