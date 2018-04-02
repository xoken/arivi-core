-- {-# LANGUAGE OverloadedStrings #-}
module Kademlia.Instance
(
    createKademliaInstance,
    runKademliaInstance,
    Config      (..)
) where

import           Control.Concurrent           (Chan, forkIO, newChan,
                                               newEmptyMVar, putMVar, readChan,
                                               takeMVar)
import           Control.Concurrent.STM.TChan (newTChan)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger
import           Control.Monad.STM            (atomically)
import qualified Data.ByteString.Char8        as BC
import           Data.List                    (find, length, tail)
import           Data.Text                    (pack)
import           Kademlia.Node
import           Kademlia.Random
import           Kademlia.Signature           as CS
import qualified Kademlia.Types               as T
import           Kademlia.Utils
import           System.Environment

data Config = Config {
        localIpAddress  :: String
    ,   localPortNo     :: String
    ,   bootStrapPeers  :: [String]
    ,   k               :: Int
    ,   privateKey      :: String
    ,   responseTime    :: Int
    ,   threadSleepTime :: Int
    ,   logFilePath     :: String
    } deriving (Eq, Show)

data KademliaHandle = KH {
        nodeID :: T.NodeId
    ,   ksock  :: T.Socket
    ,   config :: Config
}


genPublicKey sk seed
  | not (Prelude.null sk) =  (temp,CS.getPublicKey temp)
  | otherwise             =  (temp2,temp3)
  where
    temp  = CS.hexToSecretKey (BC.pack sk)
    temp2 = CS.getSecretKey seed
    temp3 = CS.getPublicKey temp2


writeLog nodeId logChan logFilePath = forkIO $ forever $ runFileLoggingT
    logFilePath $ do

  logMsg <- liftIO $ readChan logChan
  let loc       = extractFirst2  logMsg
      logSource = extractSecond2 logMsg
      logLevel  = extractThird2  logMsg
      logStr    = extractFourth  logMsg

  logInfoN (Data.Text.pack (show logStr))

createKademliaInstance :: Config -> T.Socket -> IO KademliaHandle
createKademliaInstance cfg ksocket = do
    seed <- getRandomByteString 32
    let nid = snd $ genPublicKey (privateKey cfg) seed
    return (KH nid ksocket cfg)

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
