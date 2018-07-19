{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
    , deleteIfPeerExist
    , ifPeerExist'
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.Handler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (DeserialiseFailure,
                                                        deserialiseOrFail,
                                                        serialise)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TVar
import           Control.Exception                     (displayException)
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.STM
import qualified Data.ByteString.Lazy                  as L
import qualified Data.List                             as LL
import qualified Data.Text                             as T

-- | Sends FIND_NODE to bootstrap nodes and requires a P2P instance to get
--   local node information which are passed to P2P environment during
--   P2P instance initialization.
loadDefaultPeers :: (HasP2PEnv m, HasLogging m) => [Peer] -> m ()
loadDefaultPeers = mapConcurrently_ issueFindNode

-- | Helper function to retrieve Peer list from PayLoad
getPeerListFromPayload :: L.ByteString -> Either AriviP2PException [Peer]
getPeerListFromPayload payl = do
    let payl' = deserialiseOrFail payl :: Either DeserialiseFailure PayLoad
    case payl' of
        Left _ -> Left KademliaDeserilaiseFailiure
        Right payl'' -> do
            let msg = message payl''
                msgb = messageBody msg
            case msgb of
                FN_RESP _ pl _ -> Right pl
                _              -> Left KademliaInvalidResponse

ifPeerExist' ::
       (HasKbucket m, MonadIO m) => Arivi.P2P.Kademlia.Types.NodeId -> m Bool
ifPeerExist' nid = do
    m <- ifPeerExist nid
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
issueFindNode :: (HasP2PEnv m, HasLogging m, MonadIO m) => Peer -> m ()
issueFindNode rpeer = do
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
    $(logDebug) $
        T.pack ("Issueing Find_Node to : " ++ show rip ++ ":" ++ show ruport)
    resp <-
        Exception.try $
        sendRequestforKademlia rnid Kademlia (serialise fn_msg) ruport rip
    case resp of
        Left (e :: Exception.SomeException) ->
            $(logDebug) $
            T.append
                (T.pack "Couldn't send message : ")
                (T.pack (displayException e))
        Right resp' -> do
            addToKBucket rpeer
            case getPeerListFromPayload resp' of
                Left e ->
                    $(logDebug) $
                    T.append
                        (T.pack
                             "Couldn't deserialise message while recieving fn_resp : ")
                        (T.pack (displayException e))
                Right peerl -> do
                    $(logDebug) $
                        T.pack
                            ("Received PeerList from " ++
                             show rip ++
                             ":" ++ show ruport ++ ": " ++ show peerl)
                    peerl2 <- deleteIfPeerExist peerl
                    $(logDebug) $
                        T.pack
                            ("Received PeerList after removing exisiting peers : " ++
                             show peerl2)
                    -- | Deletes nodes from peer list which already exists in
                    --   k-bucket this is important otherwise it will be stuck
                    --   in a loop where the function constantly issue
                    --   FIND_NODE request forever.
                    alpha <- getKademliaConcurrencyFactor
                    let pl3 = LL.splitAt alpha peerl2
                    mapConcurrently_ issueFindNode $ fst pl3
                    mapConcurrently_ issueFindNode $ snd pl3
