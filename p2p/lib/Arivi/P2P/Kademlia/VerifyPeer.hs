--
-- This modules implements a new command that a kademlia node can issue
-- called Verify_Peer. All kademlia node except for bootstrap node starts with
-- status verified, therefore all other nodes when they receive FN_RESP from
-- other nodes can issue verify peer to already verified nodes in kbucket
-- to update it's status.
--
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Arivi.P2P.Kademlia.VerifyPeer
    ( verifyPeer
    , getVerifiedPeers
    , isVerified
    , initBootStrap
    ) where

import Arivi.P2P.Exception
import Arivi.P2P.Kademlia.Kbucket
import Arivi.P2P.Kademlia.RefreshKbucket (issuePing)
import Arivi.P2P.Kademlia.RunConcurrently
import Arivi.P2P.Kademlia.Types
import Arivi.P2P.Kademlia.Utils (addListOfList, count')
import Arivi.P2P.Kademlia.XorDistance
import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import Arivi.P2P.MessageHandler.NodeEndpoint (issueKademliaRequest)
import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Arivi.Utils.Logging
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Exception
import qualified Control.Exception.Lifted as Exception (SomeException, try)
import Control.Lens
import Control.Monad (filterM)
import Control.Monad.Except
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import qualified Data.ByteString.Base16 as BS (encode)
import qualified Data.ByteString.Char8 as C
import Data.Monoid ()
import qualified Data.Text as T
import ListT (toList)
import qualified StmContainers.Map as H
import System.Random (randomRIO)

updateNodeStatus :: (HasKbucket m, MonadIO m) => NodeStatus -> NodeId -> ExceptT AriviP2PException m ()
updateNodeStatus status nid = do
    kb <- lift getKb
    liftIO $ atomically $ H.insert status nid (nodeStatusTable kb)

deleteVerifiedPeers :: (HasKbucket m, MonadIO m, HasLogging m) => [Peer] -> ExceptT AriviP2PException m [Peer]
deleteVerifiedPeers =
    filterM
        (\x -> do
             isV <- isVerified x
             case isV of
                 Verified -> return False
                 UnVerified -> return True)

initBootStrap :: (HasKbucket m, MonadIO m, HasLogging m) => Peer -> ExceptT AriviP2PException m ()
initBootStrap peer = do
    kb <- lift getKb
    liftIO $ atomically $ H.insert Verified (nodeID peer) (nodeStatusTable kb)
    $(logDebug) "BootStrap Node marked as Verified"

-- | Simple function to check the status of a Peer
isVerified :: (HasKbucket m, MonadIO m) => Peer -> ExceptT AriviP2PException m NodeStatus
isVerified peer = do
    kb <- lift getKb
    let vt = nodeStatusTable kb
        peer' = nodeID peer
    st <- liftIO $ atomically $ H.lookup peer' vt
    case st of
        Just st' -> return st'
        Nothing -> throwError KademliaInvalidPeer
        -- TODO add a new exception for peerDoesNotExit

getVerifiedPeers :: (HasKbucket m, MonadIO m) => Peer -> Int -> ExceptT AriviP2PException m [Peer]
getVerifiedPeers peerR k = do
    dnid <- getDefaultNodeId
    kb <- lift getKb
    let nid = nodeID peerR
        kbDistance = getKbIndex dnid nid
        kbm2 = getKbucket kb
        kbtemp = H.listT kbm2
    kvList <- liftIO $ atomically $ toList kbtemp
    let fl = filter (\x -> fst x == kbDistance) kvList
        fl2 = filter (\x -> fst x < kbDistance) kvList
        fl3 = filter (\x -> fst x > kbDistance) kvList
        fll = fmap snd fl <> fmap snd fl2 <> fmap snd fl3
    vpl <-
        filterM
            (\y -> do
                 st <- isVerified y
                 case st of
                     Verified -> return True
                     _ -> return False)
            (addListOfList fll)
    if length vpl > k
        then return (fst $ splitAt k vpl)
        else return vpl

-- -- | Get k-random verified peers
-- -- getRandomVerifiedNodes :: (HasKbucket m, MonadIO m) => Int -> m [Peer]
-- -- getRandomVerifiedNodes k = do
-- --     kb  <- getKb
-- --     let vt = nodeStatusTable  kb
-- --     rps <- getKRandomPeers k
-- --     mapM isVerified rps
filterPeer :: NodeId -> NodeId -> [Peer] -> [Peer]
filterPeer nid rnid peerL = result
  where
    result = filter (\x -> rXor >= getXorDistance (C.unpack $ BS.encode nid) (C.unpack $ BS.encode $ nodeID x)) peerL
    rXor = getXorDistance (C.unpack $ BS.encode rnid) (C.unpack $ BS.encode nid)

initVerification ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           [Peer] -> ExceptT AriviP2PException m Bool
initVerification peerL = do
    $(logDebug) $ T.pack (show "Issueing Ping for Verification")
    $(logDebug) $ T.append (T.pack "recieved vn_resp : ") (T.pack $ show peerL)
    peerL' <- deleteVerifiedPeers peerL
    $(logDebug) $ T.append (T.pack "Issueing ping to : ") (T.pack $ show peerL')
    bl <- lift $ runKademliaActionConcurrently issuePing peerL'
    let liveNodes = fromIntegral $ count' True bl
        minPeerResponded = (3 / 10) * fromIntegral (Prelude.length peerL)
    return $ (>=) liveNodes minPeerResponded

isVNRESPValid ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           [Peer] -> Peer -> ExceptT AriviP2PException m Bool
isVNRESPValid peerL peerR = do
    dnid <- getDefaultNodeId
    let temp = filterPeer dnid (nodeID peerR) peerL
        minLessPeer = (1 / 10) * fromIntegral (Prelude.length peerL)
      -- TODO address conditions when (In) is retruned or not
    let firstCheck
            | Peer dnid undefined `elem` temp = True
            | fromIntegral (Prelude.length temp) >= minLessPeer = True
            | Prelude.null temp = False
            | otherwise = False
    if firstCheck
        then initVerification peerL
        else return False

issueVerifyNode ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Peer -> Peer -> Peer -> m [Peer]
issueVerifyNode peerV peerT peerR = do
    nc@NetworkConfig {..} <- (^. networkConfig) <$> ask
      -- TODO randomly select a verified node and not as a parameter
    let vnid = nodeID peerV
        vnep = nodeEndPoint peerV
        vuport = Arivi.P2P.Kademlia.Types.udpPort vnep
        vip = nodeIp vnep
        tnid = nodeID peerT
        tnep = nodeEndPoint peerT
        tuport = Arivi.P2P.Kademlia.Types.udpPort tnep
        ttport = Arivi.P2P.Kademlia.Types.tcpPort tnep
        tip = nodeIp tnep
        tnc = NetworkConfig tnid tip tuport ttport
        vnc = NetworkConfig vnid vip vuport vuport
        vmsg = packVerifyMsg nc tnc (nodeID peerR)
    $(logDebug) $ T.pack ("Issueing Verify_Node for : " ++ show tip ++ ":" ++ show tuport)
    resp <- runExceptT $ issueKademliaRequest vnc (KademliaRequest vmsg)
    $(logDebug) $ T.pack ("Recieved Verify_Resp for : " ++ show tip ++ ":" ++ show tuport)
    case resp of
        Left e -> throw e
        -- TODO isue verifyNode once more just to be sure
        Right (KademliaResponse payload) -> do
            $(logDebug) $ T.append (T.pack "VN_RESP MSG : ") (T.pack $ show payload)
            case messageBody (message payload) of
                VN_RESP _ pl' _ -> return pl'
                _ -> throw KademliaInvalidResponse

getRandomVerifiedPeer :: (HasKbucket m, MonadIO m) => ExceptT AriviP2PException m Peer
getRandomVerifiedPeer = do
    kb <- lift getKb
    let vt = nodeStatusTable kb
        st = H.listT vt
    dnid <- getDefaultNodeId
    kvList <- liftIO $ atomically $ toList st
    let kvList' = filter (\x -> fst x /= dnid) kvList
    let vPeers =
            filter
                (\x ->
                     case snd x of
                         Verified -> True
                         _ -> False)
                kvList'
    rIndex <- liftIO $ randomRIO (0, Prelude.length vPeers - 1)
    let rp = fst $ vPeers !! rIndex
    getPeerByNodeId rp

responseHandler ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Either SomeException [Peer] -> Peer -> Peer -> ExceptT AriviP2PException m ()
responseHandler resp peerR peerT =
    case resp of
        Right pl -> do
            kb <- lift getKb
            rl <- isVNRESPValid pl peerR
            if rl
                then updateNodeStatus Verified (nodeID peerT)
                else do
                    moveToHardBound peerT
                    updateNodeStatus UnVerified (nodeID peerT)
    -- Logs the NodeStatus Table
            let kbm2 = nodeStatusTable kb
                kbtemp = H.listT kbm2
            kvList <- liftIO $ atomically $ toList kbtemp
            $(logDebug) $ T.append (T.pack "NodeStatusTable after adding : ") (T.pack (show kvList))
        Left (e :: Exception.SomeException) -> $(logDebug) (T.pack (show e))

sendVNMsg ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Peer -> Peer -> Peer -> ExceptT AriviP2PException m ()
sendVNMsg peerT peerV peerR = do
    resp <- lift $ Exception.try $ issueVerifyNode peerV peerT peerR
    t <- async $ responseHandler resp peerR peerT
    wait t

verifyPeer ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Peer -> ExceptT AriviP2PException m ()
verifyPeer peerT = do
    void $ isVerified peerT
    $(logDebug) $ T.pack "Verification Started"
    dnid <- getDefaultNodeId
    rt <- liftIO $ randomRIO (10000, 180000000)
    liftIO $ threadDelay rt
    peerV <- getRandomVerifiedPeer
    peerR <- getKClosestPeersByNodeid dnid 1
    sendVNMsg peerT peerV (head peerR)
