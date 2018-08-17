{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Arivi.P2P.PRT.Types
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different data types that are used in the Peer
-- Reputation management
--
--------------------------------------------------------------------------------
module Arivi.P2P.PRT.Types
    ( Config(..)
    , Reputation
    , PeerDeed(..)
    , PeerReputationHistory(..)
    , PeerReputationHistoryTable
    , ServicesReputationHashMap
    , P2PReputationHashMap
    ) where

import qualified Arivi.Network.Types   as Network (NodeId)
import           Data.Aeson
import           Data.ByteString.Char8 as Char8
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import           Data.Yaml
import           GHC.Generics

-- | `Reputation` is a unit to count Peer Reputations
newtype Reputation =
    Reputation Integer
    deriving (Show, Eq, Generic)

instance Num Reputation where
    Reputation x - Reputation y = Reputation (x - y)
    Reputation x + Reputation y = Reputation (x + y)
    Reputation x * Reputation y = Reputation (x * y)
    abs (Reputation x) = Reputation (abs x)
    signum (Reputation x) = Reputation (signum x)
    fromInteger x = Reputation (fromInteger x)

-- | This table contains the brief history of Peer Deeds
data PeerReputationHistory = PeerReputationHistory
    { nodeId     :: Network.NodeId
    , nofDeeds   :: Integer
    , reputation :: Reputation
    } deriving (Show, Eq, Generic)

-- |  Type of deed Peers can do
data PeerDeed -- P2P
    = DeserialiseFaliure
    | SignatureMismatch
              -- Kademlia
    | Verified
    deriving (Show, Eq, Generic)

instance Hashable PeerDeed

type PeerReputationHistoryTable
     = HM.HashMap Network.NodeId PeerReputationHistory

type PeerDeedHashMap = HM.HashMap PeerDeed Reputation

-- | This is structure of config file that contains the details of different
-- `PeerDeeds` and their respective `Reputation`
type ServicesReputationHashMap = HM.HashMap String Reputation

type P2PReputationHashMap = HM.HashMap PeerDeed Reputation

data Config = Config
    { services :: ServicesReputationHashMap
    , p2p      :: P2PReputationHashMap
    } deriving (Show, Eq, Generic)

instance FromJSON Config

instance FromJSON Reputation

instance FromJSON PeerDeed

instance FromJSONKey PeerDeed where
    fromJSONKey =
        FromJSONKeyTextParser $ \t ->
            case t of
                "SignatureMismatch" -> pure SignatureMismatch
                "DeserialiseFaliure" -> pure DeserialiseFaliure
                "Verified" -> pure Verified
                _ -> fail $ "Cannot parse key into PeerDeed: " ++ T.unpack t
