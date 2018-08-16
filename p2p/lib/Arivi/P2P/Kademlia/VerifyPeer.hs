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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE GADTs     #-}

module Arivi.P2P.Kademlia.VerifyPeer
    ( verifyPeer
    , getVerifiedNodes
    , isVerified
    , initBootStrap
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.RefreshKbucket     (issuePing)
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.Kademlia.Utils              (count')
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import           Arivi.P2P.P2PEnv                      (HasP2PEnv)
import           Arivi.P2P.Types
import           Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import           Arivi.Utils.Logging
import           Control.Concurrent                    (threadDelay)
import           Control.Concurrent.Async.Lifted       (async, mapConcurrently,
                                                        wait)
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Lens
import           Control.Monad                         (filterM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Logger                  (logDebug)
import           Control.Monad.STM                     (atomically)
import qualified Data.ByteString.Base16                as BS (encode)
import qualified Data.ByteString.Char8                 as C
import qualified Data.Text                             as T
import           ListT                                 (toList)
import qualified STMContainers.Map                     as H
import           System.Random                         (randomRIO)

deleteVerifiedPeers ::
       (HasKbucket m, MonadIO m, HasLogging m) => [Peer] -> m [Peer]
deleteVerifiedPeers =
    filterM
        (\x -> do
             isV <- isVerified x
             case isV of
                 Right Verified   -> return False
                 Right UnVerified -> return True
                 Left _           -> return True)

initBootStrap :: (HasKbucket m, MonadIO m, HasLogging m) => Peer -> m ()
initBootStrap peer = do
    kb <- getKb
    liftIO $
        atomically $ H.insert Verified (fst $ getPeer peer) (nodeStatusTable kb)
    $(logDebug) "BootStrap Node marked as Verified"

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
        -- TODO add a new exception for peerDoesNotExit

getVerifiedNodes :: (HasKbucket m, MonadIO m) => Peer -> Int -> m [Peer]
getVerifiedNodes peerR k = do
    let nid = fst $ getPeer peerR
    -- kb <- getKb
    -- Todo think about below point
    -- ? Should this multiplier factor exist (2*k)
    plt <- getKClosestPeersByNodeid nid k
    case plt of
        Right pl ->
            filterM
                (\x -> do
                     st <- isVerified x
                     case st of
                         Right Verified -> return True
                         _              -> return False)
                pl
        Left e -> throw e

-- | Get k-random verified peers
-- getRandomVerifiedNodes :: (HasKbucket m, MonadIO m) => Int -> m [Peer]
-- getRandomVerifiedNodes k = do
--     kb  <- getKb
--     let vt = nodeStatusTable  kb
--     rps <- getKRandomPeers k
--     mapM isVerified rps
isVNRESPValid ::
       (MonadReader env m, HasNetworkConfig env NetworkConfig, HasP2PEnv m, HasLogging m)
    => [Peer]
    -> Peer
    -> m (Either AriviP2PException Bool)
isVNRESPValid peerL peerR = do
    dPeer <- getDefaultNodeId
    case dPeer of
        Right dnid -> do
            let rXor =
                    getXorDistance
                        (C.unpack $ BS.encode $ fst $ getPeer peerR)
                        (C.unpack $ BS.encode dnid)
            let temp =
                    filter
                        (\x ->
                             rXor >=
                             getXorDistance
                                 (C.unpack $ BS.encode dnid)
                                 (C.unpack $ BS.encode $ fst $ getPeer x))
                        peerL
                -- TODO address conditions when (In) is retruned or not
            let result
                    | Peer (dnid, undefined) `elem` temp = True
                    | fromIntegral (Prelude.length temp) >= minLessPeer = True
                    | Prelude.null temp = False
                    | otherwise = False
            $(logDebug) $
                T.append (T.pack "First Check : ") (T.pack $ show result)
            if result
                then do
                    $(logDebug) $ T.pack (show "Issueing Ping for Verification")
                    $(logDebug) $
                        T.append
                            (T.pack "recieved vn_resp : ")
                            (T.pack $ show peerL)
                    peerL' <- deleteVerifiedPeers peerL
                    $(logDebug) $
                        T.append
                            (T.pack "Issueing ping to : ")
                            (T.pack $ show peerL')
                    bl <- mapConcurrently issuePing peerL'
                    let liveNodes = fromIntegral $ count' True bl
                    return $ Right $ (>=) liveNodes minPeerResponded
                else return $ Right False
                -- TODO add to kbucket env
            where minPeerResponded =
                      (3 / 10) * fromIntegral (Prelude.length peerL)
                  minLessPeer = (1 / 10) * fromIntegral (Prelude.length peerL)
        Left _ -> return $ Left KademliaDefaultPeerDoesNotExists

issueVerifyNode ::
       forall m env.
       ( MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasP2PEnv m
       , HasLogging m
       )
    => Peer
    -> Peer
    -> Peer
    -> m [Peer]
issueVerifyNode peerV peerT peerR = do
    nc@NetworkConfig {..} <- (^. networkConfig) <$> ask
        -- TODO randomly select a verified node and not as a parameter
    let vnid = fst $ getPeer peerV
        vnep = snd $ getPeer peerV
        vuport = Arivi.P2P.Kademlia.Types.udpPort vnep
        vip = nodeIp vnep
        tnid = fst $ getPeer peerT
        tnep = snd $ getPeer peerT
        tuport = Arivi.P2P.Kademlia.Types.udpPort tnep
        ttport = Arivi.P2P.Kademlia.Types.tcpPort tnep
        tip = nodeIp tnep
        tnc = NetworkConfig tnid tip tuport ttport
        vnc = NetworkConfig vnid vip vuport vuport
        vmsg =
            packVerifyMsg
                nc
                tnc
                (fst $ getPeer peerR)
    $(logDebug) $
        T.pack ("Issueing Verify_Node for : " ++ show tip ++ ":" ++ show tuport)
    resp <- runExceptT $ issueKademliaRequest vnc (KademliaRequest vmsg)
    $(logDebug) $
        T.pack ("Recieved Verify_Resp for : " ++ show tip ++ ":" ++ show tuport)
    case resp of
        Left e -> throw e
            -- TODO isue verifyNode once more just to be sure
        Right (KademliaResponse payload) -> do
            $(logDebug) $
                T.append (T.pack "VN_RESP MSG : ") (T.pack $ show payload)
            case messageBody (message payload) of
                VN_RESP _ pl' _ -> return pl'
                _ -> throw KademliaInvalidResponse

getRandomVerifiedPeer :: (HasKbucket m, MonadIO m) => m Peer
getRandomVerifiedPeer = do
    kb <- getKb
    let vt = nodeStatusTable kb
        st = H.stream vt
    dnid <- getDefaultNodeId
    case dnid of
        Right dnid' -> do
            kvList <- liftIO $ atomically $ toList st
            let kvList' = filter (\x -> fst x /= dnid') kvList
            let vPeers =
                    filter
                        (\x ->
                             case snd x of
                                 Verified -> True
                                 _        -> False)
                        kvList'
            rIndex <- liftIO $ randomRIO (0, Prelude.length vPeers - 1)
            let rp = fst $ vPeers !! rIndex
            getPeerByNodeId rp
        Left _ -> throw KademliaNoVerifiedPeer

responseHandler ::
       (MonadReader env m, HasNetworkConfig env NetworkConfig, HasLogging m, HasP2PEnv m)
    => Either SomeException [Peer]
    -> Peer
    -> Peer
    -> m ()
responseHandler resp peerR peerT =
    case resp of
        Right pl -> do
            kb <- getKb
            rl <- isVNRESPValid pl peerR
            case rl of
                Right True ->
                    liftIO $
                    atomically $
                    H.insert Verified (fst $ getPeer peerT) (nodeStatusTable kb)
                Right False -> do
                    moveToHardBound peerT
                    liftIO $
                        atomically $
                        H.insert
                            UnVerified
                            (fst $ getPeer peerT)
                            (nodeStatusTable kb)
                Left e -> $(logDebug) (T.pack (show e))
         -- Logs the NodeStatus Table
            let kbm2 = nodeStatusTable kb
                kbtemp = H.stream kbm2
            kvList <- liftIO $ atomically $ toList kbtemp
            $(logDebug) $
                T.append
                    (T.pack "NodeStatusTable after adding : ")
                    (T.pack (show kvList))
        -- liftIO $ print "RH Done"
        Left (e :: Exception.SomeException) -> $(logDebug) (T.pack (show e))

sendVNMsg :: (MonadReader env m, HasNetworkConfig env NetworkConfig, HasLogging m, HasP2PEnv m) => Peer -> Peer -> Peer -> m ()
sendVNMsg peerT peerV peerR = do
    resp <- Exception.try $ issueVerifyNode peerV peerT peerR
    t <- async $ responseHandler resp peerR peerT
    wait t

verifyPeer :: (MonadReader env m, HasNetworkConfig env NetworkConfig, HasP2PEnv m, HasLogging m) => Peer -> m ()
verifyPeer peerT = do
    isV <- isVerified peerT
    case isV of
        Left _ -> do
            $(logDebug) $ T.pack "Verification Started"
            dn <- getDefaultNodeId
            case dn of
                Right dnid -> do
                    rt <- liftIO $ randomRIO (10000, 180000000)
                    liftIO $ threadDelay rt
                    peerV <- getRandomVerifiedPeer
                    peerR <- getKClosestPeersByNodeid dnid 1
                    case peerR of
                        Right peer -> sendVNMsg peerT peerV (head peer)
                            -- liftIO $ print "sendVNMSG done"
                        Left e     -> $(logDebug) (T.pack (show e))
                Left _ -> return ()
        Right _ -> return ()
