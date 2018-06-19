-- {-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Arivi.Kademlia.Instance
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Sing {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This module provides access to a kademlia instance which can be initiated
-- by supplying the appropriate configuration like size of k-buckets , the
-- alpha parameter, nodeID to be used etc.
--
module Arivi.P2P.Kademlia.Instance
    -- Config      (..),
    ( T.NodeId
    ) where

--     createKademliaInstance,
import           Arivi.Crypto.Utils.Keys.Signature as CS
import           Arivi.Crypto.Utils.Random
import qualified Arivi.P2P.Kademlia.Types          as T
import           Arivi.P2P.Kademlia.Utils
import           Control.Concurrent                (Chan, ThreadId, forkIO,
                                                    newChan, newEmptyMVar,
                                                    readChan)

import           Control.Concurrent.STM.TChan      (TChan, newTChan)
import           Control.Monad                     (forever)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Logger
import           Control.Monad.STM                 (atomically)
import qualified Data.ByteString.Char8             as BC
import           Data.Text                         (pack)
import qualified Network.Socket                    as Network (SockAddr, Socket)

data Config = Config
    { bootStrapPeers  :: [String]
    , k               :: Int
    , privateKey      :: String
    , responseTime    :: Int
    , threadSleepTime :: Int
    , logFilePath     :: String
    } deriving (Eq, Show)

data KademliaHandle = KH
    { nodeID       :: T.NodeId
    , ksock        :: Network.Socket
    , config       :: Config
    , outboundChan :: TChan (T.PayLoad, Network.SockAddr)
    }

genPublicKey :: String -> BC.ByteString -> (SecretKey, PublicKey)
genPublicKey sk seed
    | not (Prelude.null sk) = (temp, CS.getPublicKey temp)
    | otherwise = (temp2, temp3)
  where
    temp = CS.hexToSecretKey (BC.pack sk)
    temp2 = CS.getSecretKey seed
    temp3 = CS.getPublicKey temp2
-- createKademliaInstance :: Config -> Network.Socket -> IO KademliaHandle
-- createKademliaInstance cfg ksocket = do
--     seed <- getRandomByteString 32
--     outBoundChan <- atomically newTChan
--     let nid = snd $ genPublicKey (privateKey cfg) seed
--     return (KH nid ksocket cfg outBoundChan)
