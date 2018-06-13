{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.RPC.Types
	(
		ServiceId,
		ResourceList,
		ResourceToPeerMap,
		ResourceId,
		Peer(..)
	)

where

import  GHC.Generics (Generic)
import	Data.HashMap.Strict as HM
import  Control.Concurrent.STM.TVar
import  Control.Concurrent.STM.TQueue
import  Arivi.Network.Types (NodeId)

data Peer =  Peer {
    peerNodeId :: NodeId
  , peerIp:: IP
  , peerUDPPort:: Port
  , peerTCPPort:: Port
} deriving(Eq,Ord,Show,Generic)

type IP = String
type Port = Int
type ResourceId = String
type ServiceId = String
type ResourceList = [ResourceId]
type ResourceToPeerMap = HM.HashMap ResourceId (ServiceId , TQueue Peer)