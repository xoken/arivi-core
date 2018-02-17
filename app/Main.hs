module Main where

import           Control.Concurrent (forkIO, newChan, newEmptyMVar, putMVar,
                                     readChan, takeMVar, threadDelay, writeChan)
import           Node
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  done <- newEmptyMVar
        -- Runs these functions on different threads
  forkIO $ runUDPServerForever (args !! 0) (args !! 1) >> putMVar done ()
  threadDelay 100000 -- wait one second
  takeMVar done
