{-# LANGUAGE DuplicateRecordFields #-}
<<<<<<< HEAD
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
=======
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.Kademlia.LoadReputedPeers
    ( loadReputedPeers
    , findGivenNode
    ) where

<<<<<<< HEAD
-- import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.LoadDefaultPeers   (getPeerListFromPayload)
import           Arivi.P2P.Kademlia.RunConcurrently
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.HandlerTypes hiding (NodeId)
import           Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
-- import           Arivi.Utils.Logging

-- import           Control.Concurrent.Async.Lifted
import           Control.Exception                     (displayException)

-- import qualified Control.Exception.Lifted              as Exception (SomeException,
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.List                             as LL

-- import           Arivi.P2P.Exception

-- import           Data.Maybe                            (fromJust)
import qualified Data.Text                             as T

loadReputedPeers ::
       forall env m r t rmsg pmsg.
       ( MonadReader env m
       , HasP2PEnv env m r t rmsg pmsg
       )
    => [Arivi.P2P.Kademlia.Types.NodeId]
    ->  m ()
=======
import Arivi.P2P.Kademlia.Kbucket
import Arivi.P2P.Kademlia.LoadDefaultPeers (getPeerListFromPayload)
import Arivi.P2P.Kademlia.RunConcurrently
import Arivi.P2P.Kademlia.Types
import Arivi.P2P.MessageHandler.HandlerTypes hiding (NodeId)
import Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Codec.Serialise
import Control.Exception (displayException)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List as LL
import qualified Data.Text as T

loadReputedPeers ::
       forall env m r t rmsg pmsg. (Serialise pmsg, Show t)
    => (MonadReader env m, HasP2PEnv env m r t rmsg pmsg) =>
           [Arivi.P2P.Kademlia.Types.NodeId] -> m ()
>>>>>>> breaking out arivi-core from arivi
loadReputedPeers nodeIdList = do
    rpeerList <- mapM (`getKClosestPeersByNodeid'` 10) nodeIdList
    let temp = zip nodeIdList rpeerList
    mapM_ (\x -> mapM_ (findGivenNode (fst x)) (snd x)) temp

<<<<<<< HEAD
getKClosestPeersByNodeid' :: (HasKbucket m, MonadIO m) => NodeId -> Int
                        -> m [Peer]
=======
getKClosestPeersByNodeid' :: (HasKbucket m, MonadIO m) => NodeId -> Int -> m [Peer]
>>>>>>> breaking out arivi-core from arivi
getKClosestPeersByNodeid' nid k = do
    temp <- runExceptT $ getKClosestPeersByNodeid nid k
    case temp of
        Left _ -> return []
        Right pl -> return pl

<<<<<<< HEAD

findGivenNode ::
       forall env m r t rmsg pmsg.
       ( MonadReader env m
       , HasP2PEnv env m r t rmsg pmsg
       )
    => Arivi.P2P.Kademlia.Types.NodeId
    -> Peer
    -> m ()
findGivenNode tnid rpeer = do
    nc@NetworkConfig {..} <- asks (^. networkConfig)
    let rnid = fst $ getPeer rpeer
        rnep = snd $ getPeer rpeer
=======
findGivenNode ::
       forall env m r t rmsg pmsg. (Serialise pmsg, Show t)
    => (MonadReader env m, HasP2PEnv env m r t rmsg pmsg) =>
           Arivi.P2P.Kademlia.Types.NodeId -> Peer -> m ()
findGivenNode tnid rpeer = do
    nc@NetworkConfig {..} <- asks (^. networkConfig)
    let rnid = nodeID rpeer
        rnep = nodeEndPoint rpeer
>>>>>>> breaking out arivi-core from arivi
        ruport = Arivi.P2P.Kademlia.Types.udpPort rnep
        rip = nodeIp rnep
        rnc = NetworkConfig rnid rip ruport ruport
        fn_msg = packFindMsg nc tnid
<<<<<<< HEAD
    $(logDebug) $
        T.pack
            ("Issuing Find_Given_Node to : " ++ show rip ++ ":" ++ show ruport)
=======
    $(logDebug) $ T.pack ("Issuing Find_Given_Node to : " ++ show rip ++ ":" ++ show ruport)
>>>>>>> breaking out arivi-core from arivi
    resp <- runExceptT $ issueKademliaRequest rnc (KademliaRequest fn_msg)
    return ()
    case resp of
        Left e -> $(logDebug) $ T.pack (displayException e)
        Right (KademliaResponse payload)
            -- _ <- runExceptT $ addToKBucket rpeer
         ->
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
<<<<<<< HEAD
                        T.pack
                            ("Received PeerList from " ++
                             show rip ++
                             ":" ++ show ruport ++ ": " ++ show peerl)
                    let peerDetail =
                            LL.find (\x -> (fst . getPeer) x == tnid) peerl
=======
                        T.pack ("Received PeerList from " ++ show rip ++ ":" ++ show ruport ++ ": " ++ show peerl)
                    let peerDetail = LL.find (\x -> (nodeID) x == tnid) peerl
>>>>>>> breaking out arivi-core from arivi
                    case peerDetail of
                        Just details -> do
                            action <- runExceptT $ addToKBucket details
                            case action of
                                Left e -> do
                                    $(logDebug) $
<<<<<<< HEAD
                                        T.append
                                            (T.pack
                                                 "Couldn't Find the node ")
                                            (T.pack (displayException e))
                                    runKademliaActionConcurrently_
                                        (findGivenNode tnid)
                                        peerl
                                Right _ ->
                                    $(logDebug) $
                                    T.pack
                                        ("Added the Peer with nodeId" ++
                                         show tnid ++ "to KBucket")
                        Nothing ->
                            runKademliaActionConcurrently_
                                (findGivenNode tnid)
                                peerl
=======
                                        T.append (T.pack "Couldn't Find the node ") (T.pack (displayException e))
                                    runKademliaActionConcurrently_ (findGivenNode tnid) peerl
                                Right _ ->
                                    $(logDebug) $ T.pack ("Added the Peer with nodeId" ++ show tnid ++ "to KBucket")
                        Nothing -> runKademliaActionConcurrently_ (findGivenNode tnid) peerl
>>>>>>> breaking out arivi-core from arivi
