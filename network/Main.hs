{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection           (Connection (..),
                                                     ConnectionId)
import           Arivi.Network.Datagram
import           Arivi.Network.Instance
import           Arivi.Network.StreamServer
import qualified Arivi.Network.Types                as ANT (PersonalityType (INITIATOR),
                                                            TransportType (..))
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.Async
import qualified Control.Concurrent.Async.Lifted    as LA
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.HashTable.IO                  as MutableHashMap (new)
-- import           Arivi.Network.Datagram             (createUDPSocket)
import           Control.Concurrent.STM.TVar        (newTVarIO)
import           Data.ByteString.Char8              (pack)
import           Data.ByteString.Lazy               (ByteString, fromStrict)
import           Data.Time
import           Debug.Trace
import           Network.Socket                     (SocketType (..))

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasEnv AppM where
  getEnv = ask

instance HasAriviNetworkInstance AppM where
  getAriviNetworkInstance = ariviNetworkInstance <$> getEnv

instance HasSecretKey AppM where
  getSecretKey = cryptoEnvSercretKey . ariviCryptoEnv <$> getEnv

instance HasLogging AppM where
  getLoggerChan = loggerChan <$> getEnv

instance HasUDPSocket AppM where
  getUDPSocket = udpSocket <$> getEnv


instance HasUDPListnerStatusTVar AppM where
  getUDPListnerStatusTVar = udpListnerStatusTVar <$> getEnv



runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT



sender :: SecretKey -> SecretKey -> IO ()
sender sk rk = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  mutableConnectionHashMap <- MutableHashMap.new
                                    :: IO (HashTable ConnectionId Connection)
  mUDPListerStatusTVar <- newTVarIO  False
  env' <- mkAriviEnv
  let env = env' { ariviCryptoEnv = CryptoEnv sk
                 , loggerChan = tq
                 -- , udpSocket = sock
                 , udpConnectionHashMap = mutableConnectionHashMap
                 , udpListnerStatusTVar = mUDPListerStatusTVar
                 }
  runStdoutLoggingT $ runAppM env (do

                                       let ha = "127.0.0.1"
                                       cidOrFail <- openConnection ha 4700 4000 ANT.UDP (generateNodeId rk) ANT.INITIATOR
                                       -- cidOrFail1 <- openConnection ha 8082 4000 ANT.UDP (generateNodeId rk) ANT.INITIATOR
                                       -- case cidOrFail of
                                       --    Left e ->  (return())
                                       --    Right cid -> do
                                       --      case cidOrFail1 of
                                       --        Left e -> return ()
                                       --        Right cid1 -> do
                                       --          time <- liftIO $ getCurrentTime
                                       --          liftIO $ print time
                                       --          mapM_ (\_ -> (sendMessage cid a) `LA.concurrently` (sendMessage cid1 a)) [1..2]
                                       --          time2 <- liftIO $ getCurrentTime
                                       --          liftIO $ print time2

                                       -- case cidOrFail of
                                       --    Left e -> return ()
                                       --    Right cid -> do
                                       --      time <- liftIO $ getCurrentTime
                                       --      liftIO $ print time
                                       --      mapM_ (\_ -> (sendMessage cid a) `LA.concurrently` (sendMessage cid a)) [1..2]
                                       --      time2 <- liftIO $ getCurrentTime
                                       --      liftIO $ print time2
                                       liftIO $ print "done"
                                   )
receiver :: SecretKey -> IO ()
receiver sk = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  mutableConnectionHashMap1 <- MutableHashMap.new
                                    :: IO (HashTable ConnectionId Connection)
  env' <- mkAriviEnv
  let env = env' { ariviCryptoEnv = CryptoEnv sk
                 , loggerChan = tq
                 -- , udpSocket = sock
                 , udpConnectionHashMap = mutableConnectionHashMap1
                 }
  runStdoutLoggingT $ runAppM env ( do
                                       -- runTCPserver (show (envPort env))
                                       mSocket <- liftIO $ makeSocket "127.0.0.1" 4700 Datagram
                                       runUDPServerForever mSocket
                                  )


-- receiver1 :: SecretKey -> IO ()
-- receiver1 sk = do
--   tq <- newTQueueIO :: IO LogChan
--   -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
--   mutableConnectionHashMap1 <- MutableHashMap.new
--                                     :: IO (HashTable ConnectionId Connection)
--   env' <- mkAriviEnv
--   let env = env' { ariviCryptoEnv = CryptoEnv sk
--                  , loggerChan = tq
--                  -- , udpSocket = sock
--                  , udpConnectionHashMap = mutableConnectionHashMap1
--                  , envPort = 8082
--                  }
--   runStdoutLoggingT $ runAppM env (
--                                        -- runTCPserver (show (envPort env))
--                                        runUDPserver (show (envPort env))
--                                   )

main :: IO ()
main = do
  (sender_sk, _) <- generateKeyPair
  (recv_sk, _) <- generateKeyPair
  a <- receiver recv_sk  `concurrently` (threadDelay 1000000 >> sender sender_sk recv_sk) -- `concurrently` (receiver1 recv_sk)
  -- (threadDelay 1000000 >> sender sender_sk recv_sk)
  print a
  threadDelay 1000000000000
  return ()

a :: ByteString
a = fromStrict (pack (replicate 1000 'a'))


a' :: ByteString
a' = fromStrict (pack (replicate 1000 'A'))
