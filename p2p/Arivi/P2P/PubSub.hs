-- |
-- Module      :  Arivi.P2P.PubSub
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- PubSub is module for Publish Subscribe architecture of Arivi P2P Layer
--

module Arivi.P2P.PubSub
(
  --    Notifier
  --  , Subscriber
  --  , Topic(..)

) where

import           Arivi.Network.Types          ( TransportType(..), NodeId, ConnectionId)
import           Arivi.P2P.Types              ( ExpiryTime, TopicCode(..), MessageType(..),
                                                P2PMessage(..), Peer(..), ResponseCode)


-- =========================================================================================

-- import           Arivi.Kademlia.Types     (NodeId)
-- import           Arivi.Network.Connection (ConnectionId)
-- import           Data.ByteString.Char8    (ByteString)



-- -- | Notifier is synonyms for P2PConnection
-- type Notifier = (ConnectionId, NodeId)

-- -- | Subscriber is synonyms for P2PConnection
-- type Subscriber = (ConnectionId, NodeId)

-- -- | Topic contains the list of subscribers and notifiers
-- data Topic = Topic {
--                       subscribers :: [Subscriber]  -- ^ List of Subscribers to
--                                                    --   Topics
--                     , notifiers   :: [Notifier]    -- ^ List of Notifier for
--                                                    --   Topics
--                    } deriving (Show,Eq)


-- register p2pInstance topicList = undefined

-- read = undefined

-- -- disseminate
-- publish context m topic = undefined

-- subscribe peerCount topicList context = undefined



-- unSubscribe topic context = undefined

