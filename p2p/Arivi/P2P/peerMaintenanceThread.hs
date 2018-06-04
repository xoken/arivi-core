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
data ServiceCode = BlockInventory | BlockSync | PendingTransaction deriving(Eq,Ord)

type IP = String
type Port = Int
type Timeout = Float

peerMatrixMaintenance topicMatTvar maxTopic= do
    (ip,port,nodeId) <- getAvailablePeer nodeType TCP
    -- query peer for services available

    -- insert peer and services supported by peer in topicTvar
    topicMat <- atomically( readTVar topicMatTvar )


peerForTopic:: Topic -> Peer
peerForTopic topic = 