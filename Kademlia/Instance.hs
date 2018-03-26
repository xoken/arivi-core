{-# LANGUAGE OverloadedStrings #-}
module Kademlia.Instance 
(
    createKademliaInstance,
    runKademliaInstance,
    Config      (..)
) where 

import           Control.Concurrent (forkIO, newChan, newEmptyMVar, putMVar,
                                     readChan, takeMVar, threadDelay, writeChan,Chan)
import           Data.List          (find, length, tail)
import           Data.Maybe         (fromMaybe)
import           Kademlia.Node
import           System.Environment
import           System.IO 
import qualified Data.List.Split    as S (splitOn)
import           Data.Text         
import qualified Network.Socket.Internal   as M 
import           Network.Socket            hiding (recv)
import           Kademlia.Utils
import qualified Kademlia.Types as T 
import           Kademlia.Random 
import           Kademlia.Signature as CS 
import qualified Data.ByteString           as B 
import qualified Data.ByteString.Char8     as BC 

import Data.ByteArray
import Control.Concurrent.STM.TChan
import Control.Monad.STM 
import Control.Monad 
import Data.Word 
import Control.Monad.Logger 
import Control.Monad.IO.Class 

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


data KademliaInstance = KademliaInstance {
                     nodeId                 :: T.NodeId 
                ,    secretKey              :: CS.SecretKey     
                ,    port                   :: String
                ,    ip                     :: String  
                ,    kb                     :: Int 
                ,    kademliaResponseTime   :: Int 
                ,    kthreadSleepTime       :: Int 
                ,    kademliaBootStrapPeers :: [String] 
                ,    klogFilePath           :: String 
                } deriving (Show )

genPublicKey seed sk   
  | (Prelude.length sk /= 0) =  (temp,CS.getPublicKey temp)
  | otherwise                =  (temp2,temp3) 
  where 
    temp  = CS.hexToSecretKey (BC.pack sk)
    temp2 = CS.getSecretKey seed 
    temp3 = CS.getPublicKey temp2 


writeLog nodeId logChan logFilePath = forkIO $ forever $ runFileLoggingT logFilePath $ do
  
  logMsg <- liftIO $ readChan logChan 
  let loc       = extractFirst2 logMsg
      logSource = extractSecond2 logMsg 
      logLevel  = extractThird2 logMsg 
      logStr    = extractFourth logMsg 

  logInfoN (Data.Text.pack (show logStr))

createKademliaInstance cfg = do 
    -- Assing the node a NodeID which is also the public id of the node 
    seed <- (Kademlia.Random.getRandomByteString 32)
    let (sk,pk)    = genPublicKey seed (privateKey cfg)
        -- tempNodeId = pk :: T.NodeId 
    let ki = KademliaInstance pk sk (localPortNo cfg) (localIpAddress cfg) (k cfg) (responseTime cfg) (threadSleepTime cfg) (bootStrapPeers cfg) (logFilePath cfg)

    return ki 

runKademliaInstance ki threadMvar  = do 

    let workerCount = 5
    inboundChan    <- atomically $ newTChan
    outboundChan   <- atomically $ newTChan
    peerChan       <- atomically $ newTChan
    kbChan         <- atomically $ newTChan
    pendingResChan <- atomically $ newTChan  
    servChan       <- atomically $ newTChan 
    logChan        <- newChan 

    msgHandlerTids     <- mapM (messageHandler (nodeId ki) (secretKey ki) servChan inboundChan outboundChan peerChan kbChan pendingResChan logChan (kb ki)) [1..workerCount]
    networkClientTids  <- mapM (networkClient outboundChan logChan) [1..workerCount]
    addtoKbChanTids    <- mapM (addToKbChan kbChan peerChan logChan ) [1..workerCount]
    pendingResChanTids <- mapM (maintainPendingResChan pendingResChan ((fromIntegral $ kademliaResponseTime ki)::T.POSIXTime) (kthreadSleepTime ki) ) [1..workerCount]
    writeLogTids       <- writeLog (nodeId ki) logChan (klogFilePath ki)

    -- putMVar threadMvar msgHandlerTids 
    -- print msgHandlerTids 

    let peerList        = kademliaBootStrapPeers ki
        defaultPeerList = (Prelude.map convertToSockAddr peerList)

    
    case (Data.List.length defaultPeerList) of
        -- case in which the default peerList is empty i.e the node will run
        -- as a bootstrap node. 
        (0)       -> do 
                runUDPServerForever ( ip ki) (port ki) inboundChan servChan 
                 
    -- case in which the default peerList is present and thus the peer would 
    -- load the default peerList 
        otherwise -> do 

                runUDPServerForever (ip ki) (port ki) inboundChan servChan
                loadDefaultPeers (nodeId ki) (secretKey ki) (defaultPeerList) outboundChan peerChan servChan pendingResChan 
    print "End reached"


    
