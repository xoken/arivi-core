{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO, newChan, newEmptyMVar, putMVar,
                                     readChan, takeMVar, threadDelay, writeChan)
import           Data.List          (find)
import           Data.Maybe         (fromMaybe)
import           Node
import           System.Environment

import qualified Data.Configurator  as C
import           Data.Text          hiding (find)

data Config = Config
  { ipAddress :: String
  , portNo    :: String
  } deriving (Eq, Show)

readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg          <- C.load [C.Required cfgFile]
  ipAddress    <- C.require cfg "node.ipAddress"
  portNo       <- C.require cfg "node.portNo"
  return $ Config ipAddress portNo


main :: IO ()
main = do
  args <- getArgs
  let cfgFilePath =
        fromMaybe (error "Usage: ./kademlia-exe --config /file/path") $
        find (/= "--config") args
  cfg <- readConfig cfgFilePath
  done <- newEmptyMVar
        -- Runs these functions on different threads
  forkIO $ runUDPServerForever (ipAddress cfg) (portNo cfg) >> putMVar done ()
  threadDelay 100000 -- wait one second
  takeMVar done
