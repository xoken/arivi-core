<<<<<<< HEAD
-- |
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
=======
>>>>>>> breaking out arivi-core from arivi
--
-- This module provides functionality to refresh k-bucket after a fixed
-- time, this is necessary because in P2P Networks nodes go offline
-- all the time that's why it is essential to make sure that active
-- peers are prioritised, this module do that by issuing the PING request
-- to the k-bucket entries and shuffle the list based on the response
--
<<<<<<< HEAD
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
=======
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.Kademlia.RefreshKbucket
    ( refreshKbucket
    , issuePing
    ) where

<<<<<<< HEAD
import           Arivi.P2P.Kademlia.RunConcurrently
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig (..))
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv--                      (HasNodeEndpoint(..))
import           Arivi.P2P.Types
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger                  (logDebug)
import           Control.Monad.Reader
import qualified Data.List                             as L
import qualified Data.Text                             as T
=======
import Arivi.P2P.Kademlia.RunConcurrently
import Arivi.P2P.Kademlia.Types
import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv --                      (HasNodeEndpoint(..))
import Arivi.P2P.Types
import Codec.Serialise
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Text as T
>>>>>>> breaking out arivi-core from arivi

-- | Helper function to combine to lists
combineList :: [[a]] -> [[a]] -> [[a]]
combineList [] [] = []
<<<<<<< HEAD
combineList l1 l2 =
    [L.head l1 ++ L.head l2, L.head (L.tail l1) ++ L.head (L.tail l2)]
=======
combineList l1 l2 = [L.head l1 ++ L.head l2, L.head (L.tail l1) ++ L.head (L.tail l2)]
>>>>>>> breaking out arivi-core from arivi

-- | creates a new list by combining two lists
addToNewList :: [Bool] -> [Peer] -> [[Peer]]
addToNewList _ [] = [[], []]
addToNewList bl pl
    | L.null bl = [[], []]
    | length bl == 1 =
        if L.head bl
            then combineList [[L.head pl], []] (addToNewList [] [])
            else combineList [[], [L.head pl]] (addToNewList [] [])
    | otherwise =
        if L.head bl
<<<<<<< HEAD
            then combineList
                     [[L.head pl], []]
                     (addToNewList (L.tail bl) (L.tail pl))
            else combineList
                     [[], [L.head pl]]
                     (addToNewList (L.tail bl) (L.tail pl))
=======
            then combineList [[L.head pl], []] (addToNewList (L.tail bl) (L.tail pl))
            else combineList [[], [L.head pl]] (addToNewList (L.tail bl) (L.tail pl))
>>>>>>> breaking out arivi-core from arivi

-- | Issues a ping command and waits for the response and if the response is
--   is valid returns True else False
issuePing ::
<<<<<<< HEAD
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => Peer
    -> m Bool
issuePing rpeer = do
    nc <- asks (^. networkConfig)
    let rnid = fst $ getPeer rpeer
        rnep = snd $ getPeer rpeer
=======
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Peer -> m Bool
issuePing rpeer = do
    nc <- asks (^. networkConfig)
    let rnid = nodeID rpeer
        rnep = nodeEndPoint rpeer
>>>>>>> breaking out arivi-core from arivi
        ruport = Arivi.P2P.Kademlia.Types.udpPort rnep
        rip = nodeIp rnep
        ping_msg = packPing nc
        rnc = NetworkConfig rnid rip ruport ruport
<<<<<<< HEAD
    $(logDebug) $
        T.pack ("Issuing ping request to : " ++ show rip ++ ":" ++ show ruport)
    resp <- runExceptT $ issueKademliaRequest rnc (KademliaRequest ping_msg)
    $(logDebug) $
        T.pack ("Response for ping from : " ++ show rip ++ ":" ++ show ruport)
=======
    $(logDebug) $ T.pack ("Issuing ping request to : " ++ show rip ++ ":" ++ show ruport)
    resp <- runExceptT $ issueKademliaRequest rnc (KademliaRequest ping_msg)
    $(logDebug) $ T.pack ("Response for ping from : " ++ show rip ++ ":" ++ show ruport)
>>>>>>> breaking out arivi-core from arivi
    case resp of
        Left e -> do
            $(logDebug) (T.pack (displayException e))
            return False
        Right (KademliaResponse payload) ->
            case messageBody (message payload) of
                PONG _ _ -> return True
<<<<<<< HEAD
                _        -> return False
=======
                _ -> return False
>>>>>>> breaking out arivi-core from arivi

deleteIfExist :: Peer -> [Peer] -> [Peer]
deleteIfExist peerR pl =
    if peerR `elem` pl
<<<<<<< HEAD
        then L.deleteBy
                 (\p1 p2 -> fst (getPeer p1) == fst (getPeer p2))
                 peerR
                 pl
        else pl

-- | creates a new list from an existing one by issuing a ping command
refreshKbucket ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => Peer
    -> [Peer]
    -> m [Peer]
=======
        then L.deleteBy (\p1 p2 -> (nodeID p1) == (nodeID p2)) peerR pl
        else pl

--(\p1 p2 -> nodeID p1 == nodeID p2)
-- | creates a new list from an existing one by issuing a ping command
refreshKbucket ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           Peer -> [Peer] -> m [Peer]
>>>>>>> breaking out arivi-core from arivi
refreshKbucket peerR pl = do
    sb <- getKb
    let pl2 = deleteIfExist peerR pl
    if L.length pl2 > pingThreshold sb
        then do
            let sl = L.splitAt (kademliaSoftBound sb) pl2
<<<<<<< HEAD
            $(logDebug) $
                T.append
                    (T.pack "Issueing ping to refresh kbucket no of req sent :")
                    (T.pack (show (fst sl)))
            resp <- runKademliaActionConcurrently issuePing (fst sl)
            $(logDebug) $
                T.append (T.pack "Pong response recieved ") (T.pack (show resp))
=======
            $(logDebug) $ T.append (T.pack "Issueing ping to refresh kbucket no of req sent :") (T.pack (show (fst sl)))
            resp <- runKademliaActionConcurrently issuePing (fst sl)
            $(logDebug) $ T.append (T.pack "Pong response recieved ") (T.pack (show resp))
>>>>>>> breaking out arivi-core from arivi
            let temp = addToNewList resp (fst sl)
                newpl = L.head temp ++ [peerR] ++ L.head (L.tail temp) ++ snd sl
            return newpl
        else return (pl2 ++ [peerR])
