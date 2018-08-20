--------------------------------------------------------------------------------
-- |
-- Module      : Arivi.P2P.PRT.Exceptions
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module defines different types of Exceptions that comes in Peer
-- Reputation management
--
--------------------------------------------------------------------------------
module Arivi.P2P.PRT.Exceptions
    ( PRTExecption(..)
    ) where

import           Control.Exception

-- | Types of Exceptions that comes in Peer Reputation management
data PRTExecption =
    PeerDeedNotFound -- ^ Throw this Exception whene given Peer NodeId not
                     --   found in the PeerReputation hashmap
    deriving (Show)

instance Exception PRTExecption
