{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

-- |
-- Module      : Arivi.P2P.Kademlia.LoadDefaultPeers
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
    , deleteIfPeerExist
    , ifPeerExist'
    , getPeerListFromPayload
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.RunConcurrently
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Control.Exception                     (displayException)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Text                             as T

-- | Sends FIND_NODE to bootstrap nodes and requires a P2P instance to get
--   local node information which are passed to P2P environment during
--   P2P instance initialization.
loadDefaultPeers ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => [Peer]
    -> m ()
loadDefaultPeers = runKademliaActionConcurrently_ issueFindNode

-- | Helper function to retrieve Peer list from PayLoad
getPeerListFromPayload :: PayLoad -> Either AriviP2PException [Peer]
getPeerListFromPayload payload =
    let msg = message payload
        msgb = messageBody msg
     in case msgb of
            FN_RESP _ pl _ -> Right pl
            _              -> Left KademliaInvalidResponse

ifPeerExist' ::
       (HasKbucket m, MonadIO m) => Arivi.P2P.Kademlia.Types.NodeId -> m Bool
ifPeerExist' nid = do
    m <- runExceptT $ ifPeerExist nid
    case m of
        Right x -> return x
        Left _  -> return False

deleteIfPeerExist :: (HasKbucket m, MonadIO m) => [Peer] -> m [Peer]
deleteIfPeerExist [] = return []
deleteIfPeerExist (x:xs) = do
    ife <- ifPeerExist' (fst $ getPeer x)
    t <- deleteIfPeerExist xs
    if not ife
        then return (x : t)
        else return []

-- | Issues a FIND_NODE request by calling the network apis from P2P Layer
--  TODO : See if need to be converted to ExceptT
issueFindNode ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => Peer
    -> m ()
issueFindNode rpeer = do
    nc@NetworkConfig {..} <- asks (^. networkConfig)
    let rnid = fst $ getPeer rpeer
        rnep = snd $ getPeer rpeer
        ruport = Arivi.P2P.Kademlia.Types.udpPort rnep
        rip = nodeIp rnep
        rnc = NetworkConfig rnid rip ruport ruport
        fn_msg = packFindMsg nc _nodeId
    $(logDebug) $
        T.pack ("Issuing Find_Node to : " ++ show rip ++ ":" ++ show ruport)
    resp <- runExceptT $ issueKademliaRequest rnc (KademliaRequest fn_msg)
    case resp of
        Left e -> $(logDebug) $ T.pack (displayException e)
        Right (KademliaResponse payload) -> do
            _ <- runExceptT $ addToKBucket rpeer
            case getPeerListFromPayload payload of
                Left e ->
                    $(logDebug) $
                    T.append
                        (T.pack
                             ("Couldn't deserialise message while recieving fn_resp from : " ++
                              show rip ++ ":" ++ show ruport))
                        (T.pack (displayException e))
                Right peerl -> do
                    $(logDebug) $
                        T.pack
                            ("Received PeerList from " ++
                             show rip ++
                             ":" ++ show ruport ++ ": " ++ show peerl)
                    -- Verification
                    -- TODO Rethink about handling exceptions
                    peerl2 <- deleteIfPeerExist peerl
                    $(logDebug) $
                        T.pack
                            ("Received PeerList after removing exisiting peers : " ++
                             show peerl2)
                    -- Initiates the verification process
                    --   Deletes nodes from peer list which already exists in
                    --   k-bucket this is important otherwise it will be stuck
                    --   in a loop where the function constantly issue
                    --   FIND_NODE request forever.
                    runKademliaActionConcurrently_ issueFindNode peerl2
