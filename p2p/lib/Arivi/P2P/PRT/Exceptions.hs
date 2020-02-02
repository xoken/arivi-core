--------------------------------------------------------------------------------
-- This module defines different types of Exceptions that comes in Peer
-- Reputation management
--
--------------------------------------------------------------------------------
module Arivi.P2P.PRT.Exceptions
    ( PRTExecption(..)
    ) where

import Control.Exception

-- | Types of Exceptions that comes in Peer Reputation management
data PRTExecption
    = PeerDeedNotFound -- ^ Throw this Exception whene given Peer Deed not
                       --   found in the PeerReputation hashmap
    | InvalidRatioReputedVsOther -- ^ Given Ratio  for ReputedVsOther is invalid
    | InvalidRatioKClosestVsRandom -- ^ Given Ratio  for KClosestVsRandom
                                   -- is invalid
    deriving (Show)

instance Exception PRTExecption
