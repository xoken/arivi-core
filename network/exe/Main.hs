{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( module Main
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               hiding (mapConcurrently_)
import           Control.Concurrent.Async.Lifted        (mapConcurrently_)
import           Control.Concurrent.STM.TQueue
import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Lazy                   as BSL (ByteString)
import           Data.ByteString.Lazy.Char8             as BSLC (pack)
import           Data.Time
import           System.Environment                     (getArgs)

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasEnv AppM where
    getEnv = ask

instance HasAriviNetworkInstance AppM where
    getAriviNetworkInstance = ariviEnvNetworkInstance <$> getEnv

instance HasSecretKey AppM where
    getSecretKey = cryptoEnvSecretKey . ariviEnvCryptoEnv <$> getEnv

instance HasLogging AppM where
    getLoggerChan = ariviEnvLoggerChan <$> getEnv

-- instance HasUDPSocket AppM where
--  getUDPSocket = ariviEnvdpSocket <$> getEnv

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

sender :: SecretKey -> SecretKey -> Int -> Int -> IO ()
sender sk rk n size = do
  tqSender <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  -- mutableConnectionHashMap <- MutableHashMap.new
  --                                  :: IO (HashTable ConnectionId CompleteConnection)
  envSender' <- mkAriviEnv
  let env = envSender' { ariviEnvCryptoEnv = CryptoEnv sk
                 , ariviEnvLoggerChan = tqSender
                 -- , udpSocket = sock
                 --, ariviEnvUdpConnectionHashMap = mutableConnectionHashMap
                 }
  runStdoutLoggingT $ runAppM env (do

                                       let ha = "127.0.0.1"
                                       handleOrFail <- openConnection ha 8083 UDP (generateNodeId rk)
                                       case handleOrFail of
                                          Left e -> throw e
                                          Right cHandle -> do
                                            time <- liftIO getCurrentTime
                                            liftIO $ print time
                                            mapConcurrently_ (const (send cHandle (a size))) [1..n]
                                            time2 <- liftIO getCurrentTime
                                            liftIO $ print time2
                                       liftIO $ print "done"
                                   )
receiver :: SecretKey -> IO ()
receiver sk = do
  tqReceiver <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  -- mutableConnectionHashMap1 <- MutableHashMap.new
                                    -- :: IO (HashTable ConnectionId CompleteConnection)
  envReceiver' <- mkAriviEnv
  let env = envReceiver' { ariviEnvCryptoEnv = CryptoEnv sk
                 , ariviEnvLoggerChan = tqReceiver
                 -- , udpSocket = sock
                 --, ariviEnvUdpConnectionHashMap = mutableConnectionHashMap1
                 }
  runStdoutLoggingT $ runAppM env (
                                       runUdpServer (show $ ariviEnvPort env) myAmazingHandler
                                  )

sender' :: SecretKey -> SecretKey -> Int -> Int -> IO ()
sender' sk rk n size = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  -- mutableConnectionHashMap <- MutableHashMap.new
  --                                  :: IO (HashTable ConnectionId CompleteConnection)
  env' <- mkAriviEnv
  let env = env' { ariviEnvCryptoEnv = CryptoEnv sk
                 , ariviEnvLoggerChan = tq
                 -- , udpSocket = sock
                 --, ariviEnvUdpConnectionHashMap = mutableConnectionHashMap
                 }
  runStdoutLoggingT $ runAppM env (do

                                       let ha = "127.0.0.1"
                                       handleOrFail <- openConnection ha 8083 TCP (generateNodeId rk)
                                       case handleOrFail of
                                          Left e -> throw e
                                          Right cHandle -> do
                                            time <- liftIO getCurrentTime
                                            liftIO $ print time
                                            mapConcurrently_ (const (send cHandle (a size))) [1..n]
                                            time2 <- liftIO getCurrentTime
                                            liftIO $ print time2
                                       liftIO $ print "done"
                                   )
receiver' :: SecretKey -> IO ()
receiver' sk = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  -- mutableConnectionHashMap1 <- MutableHashMap.new
                                    -- :: IO (HashTable ConnectionId CompleteConnection)
  env' <- mkAriviEnv
  let env = env' { ariviEnvCryptoEnv = CryptoEnv sk
                 , ariviEnvLoggerChan = tq
                 -- , udpSocket = sock
                 --, ariviEnvUdpConnectionHashMap = mutableConnectionHashMap1
                 }
  runStdoutLoggingT $ runAppM env (
                                       runTcpServer (show $ ariviEnvPort env) myAmazingHandler
                                  )
--initiator :: IO ()
initiator f size n = do
  let sender_sk = ACUPS.getSecretKey "ssssssssssssssssssssssssssssssss"
      recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
  _ <- threadDelay 1000000 >> f sender_sk recv_sk (read n) (read size)
  threadDelay 1000000000000
  return ()

--recipient :: IO ()
recipient f = do
  let recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
  f recv_sk
  threadDelay 1000000000000

main :: IO ()
main = do
  [size, n] <- getArgs
  _ <- (recipient receiver) `concurrently` (threadDelay 1000000 >> initiator sender size n) `concurrently` (recipient receiver') `concurrently` (threadDelay 1000000 >> initiator sender' size n)
  return ()

a :: Int -> BSL.ByteString
a n = BSLC.pack (replicate n 'a')

myAmazingHandler :: (HasLogging m, HasSecretKey m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h
