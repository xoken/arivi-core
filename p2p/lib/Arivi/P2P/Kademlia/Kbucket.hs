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
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.RefreshKbucket
import           Arivi.P2P.Kademlia.Types
import qualified Arivi.P2P.Kademlia.Utils          as U
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.P2PEnv                  (HasP2PEnv)
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Exception
import           Control.Monad                     ()
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Logger              (logDebug)
import           Control.Monad.Reader              ()
import           Control.Monad.STM
import qualified Data.List                         as L
import           Data.Maybe
import qualified Data.Text                         as T
import           ListT
import qualified STMContainers.Map                 as H

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
    $(logDebug) $ T.pack "Add to kbucket called "
    kb'' <- getKb
    lp <- getDefaultNodeId
    case lp of
        Right localPeer -> do
            let nid = fst $ getPeer peerR
                kbDistance = getKbIndex localPeer nid
                kb = getKbucket kb''
            mPeerList <- liftIO $ atomically $ H.lookup kbDistance kb
            case mPeerList of
                Just pl -> do
                    tempp <- refreshKbucket peerR pl
                    liftIO $ atomically $ H.insert tempp kbDistance kb
                Nothing -> do
                    liftIO $ atomically $ H.insert [peerR] kbDistance kb
                    $(logDebug) $ T.pack "First_Element in respective kbucket"
            -- Logs the Kbucket and sends pushes statsd metric
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
    let mPeerList = fst $ L.splitAt (kademliaSoftBound kbb'') $ fromMaybe [] pl
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
    keyl <- liftIO $ U.randomList 512
    getPeerListFromKeyList k keyl
