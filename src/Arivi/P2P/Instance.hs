-- |
-- Module      :  Arivi.P2P.Instance
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- PubSub is module for Publish Subscribe architecture of Arivi P2P Layer
--

module Arivi.P2P.Instance
(
	getAriviP2PInstance

) where


data AriviP2PHandle = AriviP2PHandle {
		  serviceRegistry :: ServiceRegistry
		, networkMessages :: TChan (ConnectionId,P2PMessage) --Contains incoming messages from the Network
		 -- TODO how to limit to only DataMessage types??
	} deriving (Eq)

getAriviP2PInstance  serviceRegistry networkMessages = rerturn (AriviP2PHandle serviceRegistry networkMessages)
