-- |
-- Module      : Arivi.Kademlia.LoadDefaultPeers
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This module kick starts the p2p instance by sending FIND_NODE request
-- to the default nodes which are read from a config file and are called
-- bootstrap nodes, in response it recieves a list of peer close to it
-- where closeness is determined by the XOR Metric and agains issues FIND_NODE
-- to the peers it has recieved.
module Arivi.P2P.Kademlia.LoadDefaultPeers
    ( loadDefaultPeers,
      deleteIfPeerExist,
      ifPeerExist'
    ) where

import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Exception
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.ByteString.Lazy                  as L

-- | Sends FIND_NODE to bootstrap nodes and requires a P2P instance to get
--   local node information which are passed to P2P environment during
--   P2P instance initialization.
loadDefaultPeers :: (HasP2PEnv m, HasLogging m) => [Peer] -> m ()
loadDefaultPeers = mapConcurrently_ issueFindNode

-- | Helper function to retrieve Peer list from PayLoad
getPeerListFromPayload :: L.ByteString -> Either AriviException [Peer]
getPeerListFromPayload payl = do
    let payl' = deserialise payl :: PayLoad
        msg = message payl'
        msgb = messageBody msg
    case msgb of
        FN_RESP _ pl _ -> Right pl
        _              -> Left KademliaInvalidResponse

ifPeerExist' :: (HasKbucket m) => Arivi.P2P.Kademlia.Types.NodeId -> m Bool
ifPeerExist' nid = do
    m <- ifPeerExist nid
    case m of
        Right x -> return x
        Left _  -> return False

deleteIfPeerExist :: (HasKbucket m) => [Peer] -> m [Peer]
deleteIfPeerExist [] = return []
deleteIfPeerExist (x:xs) = do
    ife <- ifPeerExist' (fst $ getPeer x)
    t <- deleteIfPeerExist xs
    if not ife
        then return (x:t)
        else return []

-- | Issues a FIND_NODE request by calling the network apis from P2P Layer
issueFindNode :: (HasP2PEnv m, HasLogging m) => Peer -> m ()
issueFindNode rpeer = do
    addToKBucket rpeer
    p2pInstanceTVar <- getAriviTVarP2PEnv
    p2pInstance <- liftIO $ atomically $ readTVar p2pInstanceTVar
    let lnid = selfNodeId p2pInstance
        luport = selfUDPPort p2pInstance
        lip = selfIP p2pInstance
        ltport = selfTCPPort p2pInstance
        rnid = fst $ getPeer rpeer
        rnep = snd $ getPeer rpeer
        ruport = Arivi.P2P.Kademlia.Types.udpPort rnep
        rip = nodeIp rnep
        fn_msg = packFindMsg lnid lnid lip luport ltport
    resp <-
        sendRequestforKademlia
            rnid
            Kademlia
            (serialise fn_msg)
            ruport
            rip
    let peerl  = case getPeerListFromPayload resp of
            Right x -> x
            Left  _ -> []

    peerl2 <- deleteIfPeerExist peerl

    -- | Deletes nodes from peer list which already exists in k-bucket
    --   this is important otherwise it will be stuck in a loop where the
    --   function constantly issue FIND_NODE request forever.

    mapConcurrently_ issueFindNode peerl2
