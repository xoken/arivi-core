-- |
-- Module      : Arivi.Kademlia.VerifyPeer
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This modules implements a new command that a kademlia node can issue
-- called Verify_Peer. All kademlia node except for bootstrap node starts with
-- status verified, therefor all other nodes when recieve FN_RESP from
-- other nodes can issue verify peer to already verified nodes in kbucket
-- to update it's status.
--
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Arivi.P2P.Kademlia.VerifyPeer
    ( isVerified
    , issueVerifyNode
    , getPeerListFromPayLoad
    , isVNRESPValid
    , getRandomVerifiedPeer
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.RefreshKbucket     (issuePing)
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.Kademlia.Utils              (count')
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.MessageHandler.Handler
import qualified Arivi.P2P.MessageHandler.HandlerTypes as HT
import           Arivi.P2P.P2PEnv                      (HasP2PEnv,
                                                        getAriviTVarP2PEnv)
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Codec.Serialise                       (DeserialiseFailure,
                                                        deserialiseOrFail,
                                                        serialise)

-- import           Control.Concurrent                    (ThreadId, forkIO,
--                                                         threadDelay)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TVar
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)

-- import           Control.Monad                         (filterM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (logDebug)
import           Control.Monad.STM                     (atomically)
import qualified Data.ByteString.Char8                 as C
import qualified Data.ByteString.Lazy                  as L
import qualified Data.Text                             as T
import           ListT                                 (toList)
import qualified STMContainers.Map                     as H
import           System.Random                         (randomRIO)

-- | Simple function to check the status of a Peer
isVerified ::
       (HasKbucket m, MonadIO m)
    => Peer
    -> m (Either AriviP2PException NodeStatus)
isVerified peer = do
    kb <- getKb
    let vt = nodeStatusTable kb
        peer' = fst $ getPeer peer
    st <- liftIO $ atomically $ H.lookup peer' vt
    case st of
        Just st' -> return $ Right st'
        Nothing  -> return $ Left KademliaInvalidPeer

-- getVerifiedNodes :: (HasKbucket m,MonadIO m) => Peer
--                                              -> Int
--                                              -> m [Peer]
-- getVerifiedNodes peerR k = do
--     let nid = fst $ getPeer peerR
--     kb <- getKb
--     -- Todo think about below point
--     -- ? Should this multiplier factor exist (2*k)
--     plt <- getKClosestPeersByNodeid nid (2*k)
--     case plt of
--         Right pl -> do
--             vPeers <- filterM (\x -> do
--                                     st <- isVerified x
--                                     case st of
--                                         Right Verified -> return True
--                                         _              -> return False
--                                 ) pl
--             return vPeers
--         Left e   -> throw e
getRandomVerifiedPeer :: (HasKbucket m, MonadIO m) => m Peer
getRandomVerifiedPeer = do
    kb <- getKb
    let vt = nodeStatusTable kb
        st = H.stream vt
    kvList <- liftIO $ atomically $ toList st
    let vPeers =
            filter
                (\x ->
                     case snd x of
                         Verified -> True
                         _        -> False)
                kvList
    rIndex <- liftIO $ randomRIO (0, Prelude.length vPeers)
    let rp = fst $ vPeers !! rIndex
    getPeerByNodeId rp

-- | Get k-random verified peers
-- getRandomVerifiedNodes :: (HasKbucket m,MonadIO m) => Int -> m [Peer]
-- getRandomVerifiedNodes k = do
--     kb  <- getKb
--     let vt = nodeStatusTable  kb
--     rps <- getKRandomPeers k
--     mapM isVerified rps
-- verifyPeer :: (HasP2PEnv m,MonadIO m, HasLogging m) => Peer -> m ThreadId
-- verifyPeer peerT = forkIO $ do
--     rt <- liftIO $ randomRIO (6e+7,3e+8)
--     threadDelay rt
--     peerV <- getRandomVerifiedPeer
--     resp <- issueVerifyNode peerV peerT
--     case resp of
--         True -> do
getPeerListFromPayLoad :: L.ByteString -> Either AriviP2PException [Peer]
getPeerListFromPayLoad payl = do
    let payl' = deserialiseOrFail payl :: Either DeserialiseFailure PayLoad
    case payl' of
        Left _ -> Left KademliaDeserialiseFailure
        Right payl'' -> do
            let msg = message payl''
                msgb = messageBody msg
            case msgb of
                VN_RESP _ pl _ -> Right pl
                _              -> Left KademliaInvalidResponse

isVNRESPValid ::
       (HasP2PEnv m, MonadIO m, HasLogging m) => [Peer] -> Peer -> m Bool
isVNRESPValid peerL peerR = do
    dPeer <- getDefaultNodeId
    case dPeer of
        Right dnid -> do
            let rXor =
                    getXorDistance
                        (C.unpack $ fst $ getPeer peerR)
                        (C.unpack dnid)
                pXor =
                    fmap
                        (\x ->
                             getXorDistance
                                 (C.unpack $ fst $ getPeer peerR)
                                 (C.unpack $ fst $ getPeer x))
                        peerL
                temp = filter (< rXor) pXor
            if Prelude.length temp > minClosePeer
                then do
                    bl <- mapConcurrently issuePing peerL
                    let liveNodes = count' True bl
                    return $ (>) liveNodes minPeerResponded
                else return False
                -- TODO add to kbucket env
            where minPeerResponded = 3
                  minClosePeer = 3
        Left _ -> throw KademliaInvalidPeer

issueVerifyNode ::
       forall m. (HasP2PEnv m, HasLogging m, MonadIO m)
    => Peer
    -> Peer
    -> m Bool
issueVerifyNode peerV peerT = do
    p2pInstanceTVar <- getAriviTVarP2PEnv
    p2pInstance <- liftIO $ atomically $ readTVar p2pInstanceTVar
    let lnid = selfNodeId p2pInstance
        luport = selfUDPPort p2pInstance
        lip = selfIP p2pInstance
        ltport = selfTCPPort p2pInstance
        -- TODO randomly select a verified node and not as a parameter
        vnid = fst $ getPeer peerV
        vnep = snd $ getPeer peerV
        vuport = Arivi.P2P.Kademlia.Types.udpPort vnep
        vip = nodeIp vnep
        tnid = fst $ getPeer peerT
        tnep = snd $ getPeer peerT
        tuport = Arivi.P2P.Kademlia.Types.udpPort tnep
        ttport = Arivi.P2P.Kademlia.Types.tcpPort tnep
        tip = nodeIp tnep
        vmsg = packVerifyMsg lnid tnid lip luport ltport tip tuport ttport
    $(logDebug) $
        T.pack ("Issueing Verify_Node to : " ++ show tip ++ ":" ++ show tuport)
    resp <-
        Exception.try $
        sendRequestforKademlia vnid HT.Kademlia (serialise vmsg) vuport vip
    $(logDebug) $ T.pack "VN_RESP rescieved : "
    case resp of
        Left (e :: Exception.SomeException) -> do
            $(logDebug) $ T.pack (displayException e)
            return False
            -- TODO isue verifyNode once more just to be sure
        Right _ -> return True
