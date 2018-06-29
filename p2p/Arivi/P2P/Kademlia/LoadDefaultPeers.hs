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
    ( loadDefaultPeers
    ) where

import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Exception
import           Codec.Serialise                       (deserialise, serialise)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as CL

-- | Sends FIND_NODE to bootstrap nodes and requires a P2P instance to get
--   local node information which are passed to P2P enviornment during
--   P2P instance initialization.
loadDefaultPeers :: (HasP2PEnv m) => [Peer] -> m ()
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

-- | Issues a FIND_NODE request by calling the network apis from P2P Layer
issueFindNode :: (HasP2PEnv m) => Peer -> m ()
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
            (L.toStrict $ serialise fn_msg)
            ruport
            rip
    let peerl = getPeerListFromPayload (CL.fromStrict resp)
    liftIO $ print ""
    liftIO $ print peerl
    -- case peerl of
    --   Right x -> mapConcurrently_ issueFindNode x
    --   Left  _ -> return ()
  -- | Deletes nodes from peer list which already exists in k-bucket
  --   this is important otherwise it will be stuck in a loop where the
  --   function constantly issue FIND_NODE request forever.
    -- let peerl2 = LL.deleteBy (\p -> ifPeerExist (fst $ getPeer p)) peerl
