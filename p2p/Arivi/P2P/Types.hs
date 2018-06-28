{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Arivi.P2P.Types
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different data types that are used in the P2P layer
--


module Arivi.P2P.Types
(
  AriviP2PInstance(..)
) where

import           Arivi.Network.Types (NodeId, TransportType (..))
import           Codec.Serialise     (Serialise)
import           GHC.Generics        (Generic)






type Port = Int
type IP = String




data AriviP2PInstance = AriviP2PInstance {
  selfNodeId  :: NodeId,
  selfIP      :: String,
  selfUDPPort :: Port,
  selfTCPPort :: Port
}deriving(Eq,Ord,Show,Generic)
