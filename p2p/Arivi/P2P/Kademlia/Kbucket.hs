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
-- peers.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.Kademlia.Kbucket
  (
    Kbucket,
    Peer,
    createKbucket
    -- addToKBucket,
    -- ifPeerExist
  ) where

import qualified Arivi.P2P.Kademlia.Types as T
import           Control.Monad
import qualified Data.HashTable.IO        as H
import           Data.Maybe

-- Peer information encapsulated in a single structure
newtype Peer = Peer {
                        getPeer :: (T.NodeId,T.NodeEndPoint)
                      }
                      deriving (Show)

instance Eq Peer where
  Peer (x,y) == Peer (a,b) = a == x

-- K-bucket to store peers
newtype Kbucket k v = Kbucket {
                        getKbucket :: H.CuckooHashTable k v
                      }
                      deriving (Show)

-- creates a new kbucket
-- TODO define a new class called hasKademliaInstance which will make sure
-- TODO that only a valid kademlia instance  gets a kbucket

-- Creates a new K-bucket which is a mutable hash table, and inserts the local
-- node with position 0 i.e kb index is zero since the distance of a node
-- from it's own address is zero. This will help insert the new peers into
-- kbucket with respect to the local peer

createKbucket :: Peer
              -> IO (Kbucket Int [Peer])
createKbucket localPeer = do
  m <- H.new
  H.insert m 0 [localPeer]
  return (Kbucket m)

-- checks if a peer already exists for a given K-b index
ifPeerExist :: Peer
            -> Kbucket Int [Peer]
            -> IO Bool
ifPeerExist peer kbucket = do
  let kb = getKbucket kbucket
  lp <- H.lookup kb 0
  -- TODO think of a better fallback case other than zero
  let localPeer  = fromMaybe [] lp
      -- TODO use getXorDistance
      kbDistance = 1
  pl <- H.lookup kb kbDistance
  let peerList = fromMaybe [] pl
  if peer `elem` peerList then return True else return False



-- adds a peer to a kbucket
-- addToKBucket :: Int -> (T.NodeId,T.NodeEndPoint) -> Kbucket -> IO ()
-- addToKBucket kbi peer kb = do
--   pli <- H.lookup kb kbi
--   let pl = fromMaybe [] pli
--   Control.Monad.when (ifPeerExist peer pl) $ H.insert kb kbi pl

