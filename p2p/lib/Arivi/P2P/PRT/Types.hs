{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- This module provides different data types that are used in the Peer
-- Reputation management
--
--------------------------------------------------------------------------------
module Arivi.P2P.PRT.Types
    ( Reputation
    , PeerDeed(..)
    , PeerReputationHistory(..)
    , PeerReputationHistoryTable
    , P2PReputationHashMap
    , ServicesReputationHashMap
    , Config(..)
    ) where

import qualified Arivi.Network.Types as Network (NodeId)
import Data.Aeson (FromJSON(..), FromJSONKey(..), FromJSONKeyFunction(..))
import qualified Data.HashMap.Strict as HM (HashMap)
import Data.Hashable (Hashable)
import Data.Ratio (Rational)
import qualified Data.Text as Text (unpack)
import GHC.Generics (Generic)

-- | `Reputation` is a unit to count Peer Reputations
newtype Reputation =
    Reputation Integer
    deriving (Show, Eq, Generic, Ord, Read)

instance Num Reputation where
    Reputation x - Reputation y = Reputation (x - y)
    Reputation x + Reputation y = Reputation (x + y)
    Reputation x * Reputation y = Reputation (x * y)
    abs (Reputation x) = Reputation (abs x)
    signum (Reputation x) = Reputation (signum x)
    fromInteger x = Reputation (fromInteger x)

instance FromJSON Reputation

-- |  Type of deed Peers can do
data PeerDeed
    = DeserialiseFaliure -- ^ `PeerDeed` when Packet sent by Peer can't
                         --    Deserialise
    | SignatureMismatch -- ^ `PeerDeed` when Signature of Peer not matched
    | Verified -- ^ `PeerDeed` when Kademlia Peer is  `Verified`
    deriving (Show, Eq, Generic, Read)

instance Hashable PeerDeed

instance FromJSON PeerDeed

instance FromJSONKey PeerDeed where
    fromJSONKey =
        FromJSONKeyTextParser $ \t ->
            case t of
                "SignatureMismatch" -> pure SignatureMismatch
                "DeserialiseFaliure" -> pure DeserialiseFaliure
                "Verified" -> pure Verified
                _ -> fail $ "Cannot parse key into PeerDeed: " ++ Text.unpack t

-- | This table contains the brief history of Peer Deeds
data PeerReputationHistory =
    PeerReputationHistory
        { nodeId :: Network.NodeId -- ^ NodeId of Peer
        , nofDeeds :: Integer -- ^ No of deeds Peer did till time
        , reputation :: Reputation -- ^ Based on the history `Reputation` of
                                    --   Peer
        }
    deriving (Show, Eq, Generic, Read)

-- | This hashmap contains `PeerReputationHistory` of each Peer
type PeerReputationHistoryTable = HM.HashMap Network.NodeId PeerReputationHistory

-- | This is structure of config file that contains the details of different
-- `PeerDeeds` and their respective `Reputation` for services
type ServicesReputationHashMap = HM.HashMap String Reputation

-- | This is structure of config file that contains the details of different
-- `PeerDeeds` and their respective `Reputation` for P2P
type P2PReputationHashMap = HM.HashMap PeerDeed Reputation

-- | This is the structure of config file that  stores the `PeerDeeds` and
-- their respective `Reputation`
data Config =
    Config
        { services :: ServicesReputationHashMap -- ^ `PeerDeeds` for services
        , p2p :: P2PReputationHashMap -- ^  `PeerDeeds` for P2P
        , reputedVsOther :: Rational -- ^ Ratio of reputated vs non reputated
        , kClosestVsRandom :: Rational -- ^ Ratio of closet vs random
        }
    deriving (Show, Eq, Generic)

instance FromJSON Config
