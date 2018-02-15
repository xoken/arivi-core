module Main where

import Node 
import Control.Concurrent    (forkIO, threadDelay,newEmptyMVar,putMVar,takeMVar,readChan, writeChan, newChan)
import System.Environment 

main :: IO ()
main = do 
        args <- getArgs 
        done <- newEmptyMVar
        -- Runs these functions on different threads 
        forkIO $ runUDPServerForever (args !! 0) (args !! 1) >> putMVar done ()
        threadDelay 100000 -- wait one second
        takeMVar done
       
