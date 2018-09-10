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
    , getPeerByNodeId
    , moveToHardBound
    , getPeersByNodeIds
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.RefreshKbucket
import           Arivi.P2P.Kademlia.Types
import qualified Arivi.P2P.Kademlia.Utils          as U
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.P2PEnv                  (HasP2PEnv)
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Exception                 ()
import           Control.Monad                     ()
import           Control.Monad.Except
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Logger              (logDebug)
import           Control.Monad.STM
import qualified Data.List                         as L
import           Data.Maybe
import qualified Data.Text                         as T
import           ListT                             (toList)
import qualified STMContainers.Map                 as H

-- | Gets default peer relative to which all the peers are stores in Kbucket
--   hash table based on XorDistance
getDefaultNodeId ::
       forall m. (HasKbucket m, MonadIO m)
    => ExceptT AriviP2PException m NodeId
getDefaultNodeId = do
    kbucket' <- lift getKb
    let kb = getKbucket kbucket'
    lp <- liftIO $ atomically $ H.lookup 0 kb
    let localPeer = fromMaybe [] lp
    if Prelude.null localPeer
        then throwError KademliaDefaultPeerDoesNotExists
        else return $ fst $ getPeer $ Prelude.head localPeer

-- | Gives a peerList of which a peer is part of in kbucket hashtable for any
--   given peer with respect to the default peer or local peer for which
--   the kbucket is created. If peer doesn't exist it returns an empty list
getPeerList ::
       (HasKbucket m, MonadIO m) => NodeId -> ExceptT AriviP2PException m [Peer]
getPeerList peerR = do
    kbucket'' <- lift getKb
    lp <- getDefaultNodeId
    let peer = peerR
        kbDistance = getKbIndex lp peer
    pl <- liftIO $ atomically $ H.lookup kbDistance (getKbucket kbucket'')
    return $ fromMaybe [] pl

-- |Gets Peer by Kbucket-Index (kb-index) Index
getPeerListByKIndex ::
       (HasKbucket m, MonadIO m) => Int -> ExceptT AriviP2PException m [Peer]
getPeerListByKIndex kbi = do
    kb' <- lift getKb
    peerl <- liftIO $ atomically $ H.lookup kbi (getKbucket kb')
    return $ fromMaybe [] peerl

-- |Checks if a peer already exists
ifPeerExist ::
       (HasKbucket m, MonadIO m) => NodeId -> ExceptT AriviP2PException m Bool
ifPeerExist peer = do
    mPeerList <- getPeerList peer
    return $ peer `elem` fmap (fst . getPeer) mPeerList

-- |Adds a given peer to kbucket hash table by calculating the appropriate
--  kbindex based on the XOR Distance.
addToKBucket ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => Peer
    -> ExceptT AriviP2PException m ()
addToKBucket peerR = do
    $(logDebug) $ T.pack "Add to kbucket called "
    lp <- getDefaultNodeId
    kb'' <- lift getKb
    let kb = getKbucket kb''
        kbDistance = getKbIndex lp (fst $ getPeer peerR)
    pl <- getPeerList (fst $ getPeer peerR)
    if Prelude.null pl
        then do
            liftIO $ atomically $ H.insert [peerR] kbDistance kb
            $(logDebug) $ T.pack "First_Element in respective kbucket"
        else do
            tempp <- lift $ refreshKbucket peerR pl
            liftIO $ atomically $ H.insert tempp kbDistance kb
    -- Logs the Kbucket and pushes statsd metric to collectd server
    let kbm2 = getKbucket kb''
        kbtemp = H.stream kbm2
    kvList <- liftIO $ atomically $ toList kbtemp
    $(logDebug) $
        T.append (T.pack "Kbucket after adding : ") (T.pack (show kvList))
    lift $ incrementCounter "KbucketSize"

-- | Removes a given peer from kbucket
removePeer ::
       (HasKbucket m, MonadIO m) => NodeId -> ExceptT AriviP2PException m ()
removePeer peerR = do
    kbb' <- lift getKb
    localPeer <- getDefaultNodeId
    pl <- getPeerList peerR
    let kb = getKbucket kbb'
        kbDistance = getKbIndex localPeer peerR
        pl2 = fmap (fst . getPeer) pl
    if peerR `elem` pl2
        then liftIO $
             atomically $
             H.insert
                 (L.deleteBy
                      (\p1 p2 -> fst (getPeer p1) == fst (getPeer p2))
                      fp
                      pl)
                 kbDistance
                 kb
        else liftIO $ atomically $ H.insert pl kbDistance kb
  where
    fnep = NodeEndPoint "" 0 0
    fp = Peer (peerR, fnep)

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
    -> ExceptT AriviP2PException m [Peer]
getKClosestPeersByPeer peerR k = do
    localPeer <- getDefaultNodeId
    kbbb' <- lift getKb
    let kbtemp = H.stream (getKbucket kbbb')
    kvList <- liftIO $ atomically $ toList kbtemp
    let peer = fst $ getPeer peerR
        kbi = getKbIndex localPeer peer
        tkeys = L.sort $ fmap fst kvList
        keys = (\(x, y) -> L.reverse x ++ y) (L.splitAt kbi tkeys)
    peerl <- lift $ getPeerListFromKeyList k keys
    return (L.delete peerR peerl)

-- | Gets k-closest peers to a given nodeid if k-peer exist in kbukcet being
--   queried else returns all availaible peers.
getKClosestPeersByNodeid ::
       (HasKbucket m, MonadIO m)
    => NodeId
    -> Int
    -> ExceptT AriviP2PException m [Peer]
getKClosestPeersByNodeid nid k = do
    localPeer <- getDefaultNodeId
    kbbb'' <- lift getKb
    let kbtemp = H.stream (getKbucket kbbb'')
    kvList <- liftIO $ atomically $ toList kbtemp
    let kbi = getKbIndex localPeer nid
        tkeys = L.delete 0 (L.sort $ fmap fst kvList)
        keys = (\(x, y) -> L.reverse x ++ y) (L.splitAt kbi tkeys)
    peerl <- lift $ getPeerListFromKeyList k keys
    let dnep = NodeEndPoint "" 0 0
        dpeer = Peer (nid, dnep)
        pl2 =
            L.deleteBy
                (\p1 p2 -> fst (getPeer p1) == fst (getPeer p2))
                dpeer
                peerl
    return pl2

-- | gets 'k' random peers from the kbucket for a given 'k', notice in this
--   case peers returned will not be closest peers
getKRandomPeers :: (HasKbucket m, MonadIO m) => Int -> m [Peer]
getKRandomPeers k = do
    keyl <- liftIO $ U.randomList 512
    getPeerListFromKeyList k keyl

getPeerByNodeId ::
       (HasKbucket m, MonadIO m) => NodeId -> ExceptT AriviP2PException m Peer
getPeerByNodeId nid = do
    localPeer <- getDefaultNodeId
    let kbi = getKbIndex localPeer nid
    pl <- getPeerListByKIndex kbi
    let t = filter (\x -> fst (getPeer x) == nid) pl
    if null t
        then throwError KademliaPeerDoesNotExist
        else return $ head t

getPeersByNodeIds ::
       (HasKbucket m, MonadIO m)
    => [NodeId]
    -> ExceptT AriviP2PException m [Peer]
getPeersByNodeIds = mapM getPeerByNodeId

moveToHardBound ::
       (HasKbucket m, MonadIO m, HasLogging m)
    => Peer
    -> ExceptT AriviP2PException m ()
moveToHardBound peer = do
    kb <- lift getKb
    lp' <- getDefaultNodeId
    pl' <- getPeerList (fst $ getPeer peer)
    _ <-
        liftIO $
        atomically $ do
            let pl'' = L.delete peer pl'
                tpl = L.splitAt (kademliaSoftBound kb) pl''
                hb = peer : snd tpl
                fpl = fst tpl ++ hb
                kbd = getKbIndex lp' (fst $ getPeer peer)
            H.insert fpl kbd (getKbucket kb)
    return ()
