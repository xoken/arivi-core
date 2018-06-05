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

module Arivi.Kademlia.Instance
(
    createKademliaInstance,
    runKademliaInstance,
    Config      (..),
    T.NodeId
) where

import           Arivi.Crypto.Utils.Keys.Signature as CS
import           Arivi.Crypto.Utils.Random
import qualified Arivi.Kademlia.Types              as T
import           Arivi.Kademlia.Utils
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

data Config = Config {
        bootStrapPeers  :: [String]
    ,   k               :: Int
    ,   privateKey      :: String
    ,   responseTime    :: Int
    ,   threadSleepTime :: Int
    ,   logFilePath     :: String
    } deriving (Eq, Show)

data KademliaHandle = KH {
        nodeID       :: T.NodeId
    ,   ksock        :: Network.Socket
    ,   config       :: Config
    ,   outboundChan :: TChan (T.PayLoad,Network.SockAddr)
}

genPublicKey :: String -> BC.ByteString -> (SecretKey, PublicKey)
genPublicKey sk seed
  | not (Prelude.null sk) =  (temp,CS.getPublicKey temp)
  | otherwise             =  (temp2,temp3)
  where
    temp  = CS.hexToSecretKey (BC.pack sk)
    temp2 = CS.getSecretKey seed
    temp3 = CS.getPublicKey temp2


writeLog :: Show d => T.NodeId -> Chan (a, b, c, d) -> FilePath -> IO ThreadId
writeLog _ logChan logFile = forkIO $ forever $ runFileLoggingT
    logFile $ do

  logMsg <- liftIO $ readChan logChan
  let (_, _, _, logStr) = logMsg
  logInfoN (Data.Text.pack (show logStr))

createKademliaInstance :: Config -> Network.Socket -> IO KademliaHandle
createKademliaInstance cfg ksocket = do
    seed <- getRandomByteString 32
    outBoundChan <- atomically newTChan
    let nid = snd $ genPublicKey (privateKey cfg) seed
    return (KH nid ksocket cfg outBoundChan)

runKademliaInstance :: KademliaHandle -> IO()
runKademliaInstance ki = do

    let workerCount = 5

    peerChan       <- atomically newTChan
    kbChan         <- atomically newTChan
    pendingResChan <- atomically newTChan
    logChan        <- newChan
    localSock      <- newEmptyMVar

    -- addtoKbChanTids    <- mapM (addToKbChan kbChan peerChan logChan )
    --                         [1..workerCount]

    -- pendingResChanTids <- mapM (maintainPendingResChan pendingResChan
    --                         ((fromIntegral $ kademliaResponseTime ki)
    --                         ::T.POSIXTime) (kthreadSleepTime ki) )
    --                         [1..workerCount]

    -- writeLogTids       <- writeLog (nodeId ki) logChan (klogFilePath ki)

    let peerList        = bootStrapPeers (config ki)
        defaultPeerList = Prelude.map convertToSockAddr peerList

    return ()
