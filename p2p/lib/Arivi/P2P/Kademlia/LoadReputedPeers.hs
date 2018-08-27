{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module Arivi.P2P.Kademlia.LoadReputedPeers
    (
     loadReputedPeers
     ,findGivenNode
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.Kademlia.LoadDefaultPeers   (getPeerListFromPayload)
import           Arivi.P2P.Kademlia.RunConcurrently
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Control.Concurrent.Async.Lifted
import           Control.Exception                     (displayException)
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.List                             as LL
import qualified Data.Text                             as T
import           Data.Maybe                            (fromJust)


loadReputedPeers ::
    ( MonadReader env m
    , HasNetworkConfig env NetworkConfig
    , HasP2PEnv m
    , HasLogging m
    )
    => [Arivi.P2P.Kademlia.Types.NodeId]
    -> Peer
    ->[ m () ]
loadReputedPeers nodeIdList rpeer = map (`findGivenNode` rpeer) nodeIdList



findGivenNode ::
       ( MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasP2PEnv m
       , HasLogging m
       )
    => Arivi.P2P.Kademlia.Types.NodeId
    -> Peer
    -> m()
findGivenNode tnid rpeer = do
    nc@NetworkConfig {..} <- asks (^. networkConfig)
    let rnid = fst $ getPeer rpeer
        rnep = snd $ getPeer rpeer
        ruport = Arivi.P2P.Kademlia.Types.udpPort rnep
        rip = nodeIp rnep
        rnc = NetworkConfig rnid rip ruport ruport
        fn_msg = packFindMsg nc tnid
    $(logDebug) $
        T.pack ("Issuing Find_Given_Node to : " ++ show rip ++":"++ show ruport)
    resp <- runExceptT $ issueKademliaRequest rnc (KademliaRequest fn_msg) Nothing
    case resp of
        Left e -> $(logDebug) $ T.pack (displayException e)
        Right (KademliaResponse payload) ->
            -- _ <- runExceptT $ addToKBucket rpeer
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
                    let peerDetail = LL.find (\x -> (fst . getPeer) x == tnid ) peerl
                    case peerDetail of
                        Just details -> do
                                action <-  runExceptT $ addToKBucket details
                                case action of
                                    Left e -> do
                                        $(logDebug) $
                                            T.append
                                                (T.pack
                                                ("Couldn't deserialise message while recieving fn_resp from : " ++
                                                show rip ++ ":" ++ show ruport))
                                            (T.pack (displayException e))
                                        runKademliaActionConcurrently_ (findGivenNode tnid) peerl
                                    Right _ ->
                                        $(logDebug) $
                                            T.pack
                                                ("Added the Peer with nodeId" ++ show tnid  ++"to KBucket")
                        Nothing -> runKademliaActionConcurrently_ (findGivenNode tnid) peerl


                    -- Initiates the verification process
                    --   Deletes nodes from peer list which already exists in
                    --   k-bucket this is important otherwise it will be stuck
                    --   in a loop where the function constantly issue
                    --   FIND_NODE request forever.




