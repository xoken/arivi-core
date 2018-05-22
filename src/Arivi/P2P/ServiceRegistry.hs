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

import           Arivi.Network.Stream
import           Arivi.Network.Connection     (Connection, ConnectionId)
import           Arivi.Network.Types          (TransportType (..))
import           Arivi.P2P.PubSub             (Topic)
import           Arivi.P2P.Types
import           Control.Concurrent
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Exception(try, SomeException)
import           Data.HashTable.IO
import           Data.Maybe                   (Maybe)
import           Data.UUID                    (UUID)
import           Data.UUID.V1                 (nextUUID)
import           Network(connectTo, PortID(..), PortNumber(..))
import           System.IO(Handle)
import           Arivi.Kademlia.Query
import           Arivi.Kademlia.Types

data NodeType = FullNode | HalfNode deriving(Show)

type ServiceCode = Int
type IP = String
type Port = Int
type Peers = [ (NodeId,IP,Port) ]
type PeerCount = Int
type Context = (PeerCount, NodeType, TransportType)

type ServiceContext = [ ( ServiceCode,Context ) ]
type SrvPeer = [ ( ServiceCode,Peers ) ]


makeP2Pinstance nodeId ip port = do return ()
-- -- | Generates unique context id
-- genUniqueSessionId :: IO (Maybe Data.UUID.UUID)
-- genUniqueSessionId = Data.UUID.V1.nextUUID

-- _registerService servToSessionHash sessionToConnHash ariviHandle
--                 serviceCode transportType nodeType
--                 nodeCount serviceRegistryHashMap h1 h2= do
--         let service = ServiceContext serviceCode transportType
--                                  nodeType nodeCount
--         forkIO (_connectToPeers servToSessionHash sessionToConnHash
--                      nodeCount nodeType transportType)
--         return service

-- _connectToPeers servToSessionHash sessionToConnHash nodeCount
--                nodeType transportType = if nodeCount < 1
--                 then return ()
--                 else do {
--                     (ip,port,nodeId) <- getAvailablePeer nodeType transportType
--                     handle serviceContext connection Idle InitServiceNegotiationEvent
--                     return ()
--                 }
--                 --createSocket ip port





        -- Data.HashTable.IO.insert serviceRegistryHashMap
        --                            contextId
        -- return serviceRegistryHashMap


-- TODO serviceRegistry HashMap contextId serviceContex

-- data ServiceRegistry = ServiceRegistry {
--                 hashTable     :: CuckooHashTable
--               , contextId     :: ContextId
--               , serviceContex :: ServiceContext
--             }

-- -- | ConnectionCommand keeps track of Connection Shared Variable
-- data ConnectionCommand = ConnectionCommand {
--            connectionId   :: ConnectionId   -- ^ Unique Connection Identifier
--           ,connectionMVar :: MVar Connection-- ^ Shared Variable  for connection
--           }


--      -> CuckooHashTable ServiceCode [Topic]
--      -> IO (CuckooHashTable ServiceCode [Topic])
