-- |
-- Module      : Arivi.Kademlia.Kbucket
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This module provides access to Kbucket which is responsible for storing
-- peers, and other helper functions to work with kbucket.
--
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Arivi.P2P.Kademlia.Kbucket
    ( Kbucket(..)
    , Peer(..)
    , createKbucket
    , getDefaultNodeId
    , getPeerList
    , getPeerListByKIndex
    , ifPeerExist
    , addToKBucket
    , removePeer
    , getKClosestPeersByPeer
    , getKClosestPeersByNodeid
    , getKRandomPeers
    , issuePing
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Types
import qualified Arivi.P2P.Kademlia.Utils              as U
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.MessageHandler.Handler
import qualified Arivi.P2P.MessageHandler.HandlerTypes as HT
import           Arivi.P2P.P2PEnv                      (HasP2PEnv,
                                                        getAriviTVarP2PEnv)
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Codec.Serialise                       (DeserialiseFailure,
                                                        deserialiseOrFail,
                                                        serialise)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM.TVar           (readTVar)
import           Control.Exception
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         ()
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (logDebug)
import           Control.Monad.Reader                  ()
import           Control.Monad.STM
import qualified Data.List                             as L
import           Data.Maybe
import qualified Data.Text                             as T
import           ListT
import qualified STMContainers.Map                     as H

-- | Gets default peer relative to which all the peers are stores in Kbucket
--   hash table based on XorDistance
getDefaultNodeId ::
       (HasKbucket m, MonadIO m) => m (Either AriviP2PException NodeId)
getDefaultNodeId = do
    kbucket' <- getKb
    let kb = getKbucket kbucket'
    lp <- liftIO $ atomically $ H.lookup 0 kb
    let localPeer = fromMaybe [] lp
    if Prelude.null localPeer
        then return $ Left KademliaDefaultPeerDoesNotExists
        else return $ Right $ fst $ getPeer $ Prelude.head localPeer

-- | Gives a peerList of which a peer is part of in kbucket hashtable for any
--   given peer with respect to the default peer or local peer for which
--   the kbucket is created. If peer doesn't exist it returns an empty list
getPeerList ::
       (HasKbucket m, MonadIO m)
    => NodeId
    -> m (Either AriviP2PException [Peer])
getPeerList peerR = do
    kbucket'' <- getKb
    -- liftIO (atomically (H.size (getKbucket kbucket'')) >>= print)
    lp <- getDefaultNodeId
    case lp of
        Right localPeer
            -- liftIO $ print (peerR == localPeer)
         -> do
            let peer = peerR
                kbDistance = getKbIndex localPeer peer
            pl <-
                liftIO $ atomically $ H.lookup kbDistance (getKbucket kbucket'')
            let mPeerList = fromMaybe [] pl
            return $ Right mPeerList
        Left _ -> return $ Left KademliaDefaultPeerDoesNotExists

-- |Gets Peer by Kbucket-Index (kb-index) Index
getPeerListByKIndex ::
       (HasKbucket m, MonadIO m) => Int -> m (Either AriviP2PException [Peer])
getPeerListByKIndex kbi = do
    kb' <- getKb
    peerl <- liftIO $ atomically $ H.lookup kbi (getKbucket kb')
    let pl = fromMaybe [] peerl
    case pl of
        [] -> return $ Left KademliaKbIndexDoesNotExist
        _  -> return $ Right pl

-- |Checks if a peer already exists
ifPeerExist ::
       (HasKbucket m, MonadIO m) => NodeId -> m (Either AriviP2PException Bool)
ifPeerExist peer = do
    mPeerList <- getPeerList peer
    case mPeerList of
        Right pl ->
            if peer `elem` pl2
                then return (Right True)
                else return (Right False)
            where pl2 = fmap (fst . getPeer) pl
        Left _ -> return (Left KademliaKbIndexDoesNotExist)

-- |Adds a given peer to kbucket hash table by calculating the appropriate
--  kbindex based on the XOR Distance.
addToKBucket :: (HasP2PEnv m, MonadIO m, HasLogging m) => Peer -> m ()
addToKBucket peerR = do
    kb'' <- getKb
    lp <- getDefaultNodeId
    case lp of
        Right localPeer -> do
            let nid = fst $ getPeer peerR
                kbDistance = getKbIndex localPeer nid
                kb = getKbucket kb''
            liftIO $
                atomically $ do
                    mPeerList <- H.lookup kbDistance kb
                    case mPeerList of
                        Just pl ->
                            if peerR `elem` pl
                                then do
                                    let pl2 =
                                            L.deleteBy
                                                (\p1 p2 ->
                                                     fst (getPeer p1) ==
                                                     fst (getPeer p2))
                                                peerR
                                                pl
                                    H.insert (pl2 ++ [peerR]) kbDistance kb
                                else H.insert (pl ++ [peerR]) kbDistance kb
                        Nothing -> H.insert [peerR] kbDistance kb
            -- Logs the Kbucket
            let kbm2 = getKbucket kb''
                kbtemp = H.stream kbm2
            kvList <- liftIO $ atomically $ toList kbtemp
            $(logDebug) $
                T.append
                    (T.pack "Kbucket after adding : ")
                    (T.pack (show kvList))
            incrementCounter "KbucketSize"
        Left e -> throw e

-- | Removes a given peer from kbucket
removePeer :: (HasP2PEnv m, MonadIO m, HasLogging m) => NodeId -> m ()
removePeer peerR = do
    kbb' <- getKb
    lp <- getDefaultNodeId
    case lp of
        Right localPeer -> do
            mPeerList <- getPeerList peerR
            case mPeerList of
                Right pl -> do
                    let kb = getKbucket kbb'
                        kbDistance = getKbIndex localPeer peerR
                    if peerR `elem` pl2
                        then liftIO $
                             atomically $
                             H.insert
                                 (L.deleteBy
                                      (\p1 p2 ->
                                           fst (getPeer p1) == fst (getPeer p2))
                                      fp
                                      pl)
                                 kbDistance
                                 kb
                        else liftIO $ atomically $ H.insert pl kbDistance kb
                    where pl2 = fmap (fst . getPeer) pl
                          fnep = NodeEndPoint "" 0 0
                          fp = Peer (peerR, fnep)
                Left _ -> return ()
            -- Logging
            let kbm2 = getKbucket kbb'
                kbtemp = H.stream kbm2
            kvList <- liftIO $ atomically $ toList kbtemp
            $(logDebug) $
                T.append
                    (T.pack "Kbucket after deleting : ")
                    (T.pack (show kvList))
            decrementCounter "KbucketSize"
        Left _ -> return ()

-- Gives a peer list given a list of keys
getPeerListFromKeyList :: (HasKbucket m, MonadIO m) => Int -> [Int] -> m [Peer]
getPeerListFromKeyList _ [] = return []
getPeerListFromKeyList 0 _ = return []
getPeerListFromKeyList k (x:xs) = do
    kbb'' <- getKb
    pl <- liftIO $ atomically $ H.lookup x (getKbucket kbb'')
    let mPeerList = fromMaybe [] pl
        ple = fst $ L.splitAt k mPeerList
    if L.length ple >= k
        then return ple
        else do
            temp <- getPeerListFromKeyList (k - L.length ple) xs
            return $ ple ++ temp

-- | Gets k-closest peers to a given peeer if k-peer exist in kbukcet being
--   queried else returns all availaible peers.
getKClosestPeersByPeer ::
       (HasKbucket m, MonadIO m)
    => Peer
    -> Int
    -> m (Either AriviP2PException [Peer])
getKClosestPeersByPeer peerR k = do
    lp <- getDefaultNodeId
    case lp of
        Right localPeer -> do
            kbbb' <- getKb
            let kbtemp = H.stream (getKbucket kbbb')
            kvList <- liftIO $ atomically $ toList kbtemp
            let peer = fst $ getPeer peerR
                kbi = getKbIndex localPeer peer
                tkeys = L.sort $ fmap fst kvList
                keys = (\(x, y) -> L.reverse x ++ y) (L.splitAt kbi tkeys)
            peerl <- getPeerListFromKeyList k keys
            return (Right $ L.delete peerR peerl)
        Left x -> return (Left x)

-- | Gets k-closest peers to a given nodeid if k-peer exist in kbukcet being
--   queried else returns all availaible peers.
getKClosestPeersByNodeid ::
       (HasKbucket m, MonadIO m)
    => NodeId
    -> Int
    -> m (Either AriviP2PException [Peer])
getKClosestPeersByNodeid nid k = do
    lp <- getDefaultNodeId
    case lp of
        Right localPeer -> do
            kbbb'' <- getKb
            let kbtemp = H.stream (getKbucket kbbb'')
            kvList <- liftIO $ atomically $ toList kbtemp
            let kbi = getKbIndex localPeer nid
                tkeys = L.delete 0 (L.sort $ fmap fst kvList)
                keys = (\(x, y) -> L.reverse x ++ y) (L.splitAt kbi tkeys)
            peerl <- getPeerListFromKeyList k keys
            let dnep = NodeEndPoint "" 0 0
                dpeer = Peer (nid, dnep)
                pl2 =
                    L.deleteBy
                        (\p1 p2 -> fst (getPeer p1) == fst (getPeer p2))
                        dpeer
                        peerl
            return (Right pl2)
        Left x -> return (Left x)

-- | gets 'k' random peers from the kbucket for a given 'k', notice in this
--   case peers returned will not be closest peers
getKRandomPeers :: (HasKbucket m, MonadIO m) => Int -> m [Peer]
getKRandomPeers k = do
    keyl <- liftIO $ U.randomList 255
    getPeerListFromKeyList k keyl

combineList :: [[a]] -> [[a]] -> [[a]]
combineList [] [] = []
combineList l1 l2 =
    [L.head l1 ++ L.head l2, L.head (L.tail l1) ++ L.head (L.tail l2)]

addToNewList :: [Bool] -> [Peer] -> [[Peer]]
addToNewList bl [] = [[], []]
addToNewList bl pl
    | L.null bl = [[], []]
    | length bl == 1 =
        if L.head bl
            then combineList [[L.head pl], []] (addToNewList [] [])
            else combineList [[], [L.head pl]] (addToNewList [] [])
    | otherwise =
        if L.head bl
            then combineList
                     [[L.head pl], []]
                     (addToNewList (L.tail bl) (L.tail pl))
            else combineList
                     [[], [L.head pl]]
                     (addToNewList (L.tail bl) (L.tail pl))

refreshKbucket ::
       (HasP2PEnv m, HasLogging m, MonadIO m) => Peer -> [Peer] -> m [Peer]
refreshKbucket peerR pl = do
    $(logDebug) $
        T.append
            (T.pack "Issueing ping to refresh kbucke, no of req sent :")
            (T.pack (show $ length pl))
    resp <- mapConcurrently issuePing pl
    $(logDebug) $
        T.append
            (T.pack "Pong response recieved : len : ")
            (T.pack (show $ length resp))
    let temp = addToNewList resp pl
        newpl = L.head temp ++ [peerR] ++ L.head (L.tail temp)
    return newpl

issuePing ::
       forall m. (HasP2PEnv m, HasLogging m, MonadIO m)
    => Peer
    -> m Bool
issuePing rpeer = do
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
        ping_msg = packPing lnid lip luport ltport
    resp <-
        Exception.try $
        sendRequestforKademlia rnid HT.Kademlia (serialise ping_msg) ruport rip
    case resp of
        Left (e :: Exception.SomeException) -> do
            $(logDebug) (T.pack (displayException e))
            return False
        Right resp' -> do
            let resp'' =
                    deserialiseOrFail resp' :: Either DeserialiseFailure PayLoad
            case resp'' of
                Left e -> do
                    $(logDebug) $
                        T.append
                            (T.pack "Deserilization failure: ")
                            (T.pack (displayException e))
                    return False
                Right rl -> do
                    let msg = message rl
                        msgb = messageBody msg
                    case msgb of
                        PONG _ _ -> return True
                        _        -> return False
