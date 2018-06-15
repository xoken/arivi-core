-- |
-- Module      : Arivi.Kademlia.Instance
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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Arivi.P2P.Kademlia.Kbucket
  (
    Kbucket (..),
    Peer (..),
    createKbucket,
    getDefaultNodeId,
    HasKbucket (..),
    getPeerList,
    getPeerListByKIndex,
    ifPeerExist,
    addToKBucket,
    removePeer,
    getKClosestPeers,
    getKRandomPeers
  ) where

import qualified Arivi.P2P.Kademlia.Types       as T
import qualified Arivi.P2P.Kademlia.Utils       as U
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.Utils.Exception
import           Control.Monad                  ()
import           Control.Monad.IO.Class
import           Control.Monad.Reader           ()
import           Control.Monad.STM
import           Control.Monad.Trans.Control
import qualified Data.List                      as L
import           Data.Maybe
import           ListT
import qualified STMContainers.Map              as H

-- | Peer information encapsulated in a single structure
newtype Peer = Peer {
                        getPeer :: (T.NodeId,T.NodeEndPoint)
                      }
                      deriving (Show)

instance Eq Peer where
  Peer (x,_) == Peer (a,_) = a == x

-- | K-bucket to store peers
newtype Kbucket k v = Kbucket {
                        getKbucket :: H.Map k v
                      }

class (MonadIO m, MonadBaseControl IO m) => HasKbucket m where
  getKb :: m (Kbucket Int [Peer])


-- | Creates a new K-bucket which is a mutable hash table, and inserts the local
-- node with position 0 i.e kb index is zero since the distance of a node
-- from it's own address is zero. This will help insert the new peers into
-- kbucket with respect to the local peer

createKbucket :: Peer
              -> IO (Kbucket Int [Peer])
createKbucket localPeer = do
  m <- atomically H.new
  atomically $ H.insert [localPeer] 0 m
  return (Kbucket m)

-- | Gets default peer relative to which all the peers are stores in Kbucket
--   hash table based on XorDistance
getDefaultNodeId :: (HasKbucket m) => m (Either AriviException T.NodeId)
getDefaultNodeId = do
  kbucket <- getKb
  let kb = getKbucket kbucket
  lp <- liftIO $ atomically $ H.lookup 0 kb
  let localPeer = fromMaybe [] lp
  if Prelude.null localPeer
    then return $ Left KademliaDefaultPeerDoesNotExists
    else return $ Right $ fst $ getPeer $ Prelude.head localPeer

-- | Gives a peerList of which a peer is part of in kbucket hashtable for any
--   given peer with respect to the default peer or local peer for which
--   the kbucket is created. If peer doesn't exist it returns an empty list
getPeerList ::(HasKbucket m) => Peer
            -> m (Either AriviException [Peer])
getPeerList peerR = do
  kbucket <- getKb
  lp <- getDefaultNodeId
  case lp of
    Right localPeer ->  do
                            let peer       = fst $ getPeer peerR
                                kbDistance = getKbIndex localPeer peer
                            pl <- liftIO $ atomically $ H.lookup kbDistance
                                    (getKbucket kbucket)
                            let peerList = fromMaybe [] pl
                            return $ Right peerList

    Left _          -> return $ Left KademliaDefaultPeerDoesNotExists

-- |Gets Peer by Kbucket-Index (kb-index) Index
getPeerListByKIndex :: (HasKbucket m) => Int
                    -> m (Either AriviException [Peer])
getPeerListByKIndex kbi = do
  kbucket <- getKb
  peerl <- liftIO $ atomically $ H.lookup kbi (getKbucket kbucket)
  let pl = fromMaybe [] peerl
  case pl of
    [] -> return $ Left KademliaKbIndexDoesNotExist
    _  -> return $ Right pl

-- |Checks if a peer already exists
ifPeerExist :: (HasKbucket m) => Peer
            -> m (Either AriviException Bool)
ifPeerExist peer = do
  peerList <- getPeerList peer
  case peerList of
    Right pl  -> if peer `elem` pl
                  then return (Right True)
                  else return (Right False)
    Left  _   -> return (Left KademliaKbIndexDoesNotExist)

-- |Adds a given peer to kbucket hash table by calculating the appropriate
--  kbindex based on the XOR Distance.
addToKBucket :: (HasKbucket m) => Peer
             -> m ()
addToKBucket peerR = do
  kbucket <- getKb
  lp <- getDefaultNodeId
  case lp of
    Right localPeer -> do
      peerList <- getPeerList peerR
      case peerList of
        Right pl -> do
          let kb = getKbucket kbucket
          let peer       = fst $ getPeer peerR
              kbDistance = getKbIndex localPeer peer
          if peerR `elem` pl
            then do
              removePeer peerR
              liftIO $ atomically $ H.insert (pl ++ [peerR]) kbDistance kb
            else liftIO $ atomically $ H.insert [peerR] kbDistance kb
        Left _ -> return ()
    Left _ -> return ()

-- | Removes a given peer from kbucket
removePeer :: (HasKbucket m) => Peer
           -> m ()
removePeer peerR = do
  kbucket <- getKb
  lp <- getDefaultNodeId
  case lp of
    Right localPeer -> do
      peerList <- getPeerList peerR
      case peerList of
        Right pl -> do
          let kb = getKbucket kbucket
              peer       = fst $ getPeer peerR
              kbDistance = getKbIndex localPeer peer
          if peerR `elem` pl
            then liftIO $ atomically $ H.insert (L.delete peerR pl) kbDistance kb
            else liftIO $ atomically $ H.insert [peerR] kbDistance kb
        Left _ -> return ()
    Left _ -> return ()

-- Gives a peer list given a list of keys
getPeerListFromKeyList ::(HasKbucket m) => Int
                       -> [Int]
                       -> m [Peer]
getPeerListFromKeyList _ []     = return []
getPeerListFromKeyList 0 _      = return []
getPeerListFromKeyList k (x:xs) = do
  kbucket <- getKb
  pl <- liftIO $ atomically $ H.lookup x (getKbucket kbucket)
  let peerList = fromMaybe [] pl
      ple      = fst $ L.splitAt k peerList
  if L.length peerList >= k then return ple else do
      temp <- getPeerListFromKeyList (k- L.length ple) xs
      return $ ple ++ temp

-- | Gets k-closest peers to a given peeer if k-peer exist in kbukcet being
--   queried else returns all availaible peers.
getKClosestPeers ::(HasKbucket m) => Peer
                 -> Int
                 -> m (Either AriviException [Peer])
getKClosestPeers peerR k = do
  lp <- getDefaultNodeId
  case lp of
    Right localPeer -> do
      kbucket <- getKb
      let kbtemp = H.stream (getKbucket kbucket)
      kvList <- liftIO $ atomically $ toList kbtemp
      let  peer   = fst $ getPeer peerR
           kbi    = getKbIndex localPeer peer
           tkeys  = L.sort $ fmap fst kvList
           keys   = (\(x,y) -> L.reverse x ++ y) (L.splitAt kbi tkeys)
      peerl <- getPeerListFromKeyList k keys
      return (Right peerl)
    Left x  -> return (Left x)

-- | gets 'k' random peers from the kbucket for a given 'k', notice in this
--   case peers returned will not be closest peers
getKRandomPeers :: (HasKbucket m) => Peer
                -> Int
                -> m [Peer]
getKRandomPeers peerR k = do
  keyl <- liftIO $ U.randomList 255
  getPeerListFromKeyList k keyl
