{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Concurrent (forkIO, newChan, newEmptyMVar, putMVar,
                                     readChan, takeMVar, threadDelay, writeChan)
import           Data.List          (find, length, tail)
import           Data.Maybe         (fromMaybe)
import           Node
import           System.Environment
import           System.IO 
import qualified Data.Configurator  as C
import qualified Data.List.Split    as S (splitOn)
import           Data.Text          hiding (find)

import qualified Network.Socket.Internal   as M 
import           Network.Socket            hiding (recv)
import           Utils 


-- Custom data type to collect data from configutation file 
data Config = Config
  { localIpAddress :: String
  , localPortNo    :: String
  , bootStrapPeers :: [String]
  } deriving (Eq, Show)

readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  localIpAddress    <- C.require cfg "node.localIpAddress"
  localPortNo       <- C.require cfg "node.localPortNo"
  bootStrapPeers    <- C.require cfg "node.bootStrapPeers"
  return $ Config localIpAddress localPortNo bootStrapPeers

--extractTupleFromConfig [] = ("0.0.0":"0")
extractTupleFromConfig :: String -> (String,String)
extractTupleFromConfig [] = ("","")
extractTupleFromConfig x = (peer_Ip,peer_Port)
  where
    peerInfo = S.splitOn ":" x
    peer_Ip = (peerInfo !! 0)
    peer_Port = (peerInfo !! 1)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let cfgFilePath =
        fromMaybe (error "Usage: ./kademlia-exe --config /file/path") $
        find (/= "--config") args

      args1 = Data.List.tail args
      isBsNode =
        fromMaybe (error "Usage ./kademlia-exe --isbsnode yes/no") $
        find (/= "--bsnode") $ Data.List.tail args1

  -- reads configuration file
  cfg <- readConfig cfgFilePath

  let workerCount = 5
  inboundChan  <- newChan
  outboundChan <- newChan
  peerChan     <- newChan
  kbChan       <- newChan
  servChan     <- newChan 
  
  mapM_ (messageHandler inboundChan outboundChan peerChan) [1..workerCount]
  mapM_ (networkClient outboundChan ) [1..workerCount]
  mapM_ (addToKbChan kbChan peerChan ) [1..workerCount]

  -- Allow First node to be run as a bootstrap node because there are no peers to connect to
  case isBsNode of
    "y" -> do
      done <- newEmptyMVar
      forkIO $ runUDPServerForever (localIpAddress cfg) (localPortNo cfg) inboundChan servChan >> putMVar done ()
      takeMVar done
    
    "n" ->  do
      done <- newEmptyMVar
      let peerList = bootStrapPeers cfg
          defaultPeerList = (Prelude.map convertToSockAddr peerList)
  
      forkIO $ runUDPServerForever (localIpAddress cfg) (localPortNo cfg) inboundChan servChan >> putMVar done ()
      forkIO $ loadDefaultPeers (defaultPeerList) outboundChan peerChan servChan >> putMVar done ()
        
      takeMVar done
      takeMVar done