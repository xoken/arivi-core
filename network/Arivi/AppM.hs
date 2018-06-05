{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arivi.AppM (module Arivi.AppM) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection           (Connection (..),
                                                     ConnectionId)
import           Arivi.Network.Instance
import           Arivi.Network.StreamServer
import qualified Arivi.Network.Types                as ANT (PersonalityType (INITIATOR),
                                                            TransportType (TCP))
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.HashTable.IO                  as MutableHashMap (new)
-- import           Arivi.Network.Datagram             (createUDPSocket)

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

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT



sender :: SecretKey -> SecretKey -> IO ()
sender sk rk = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  mutableConnectionHashMap <- MutableHashMap.new
                                    :: IO (HashTable ConnectionId Connection)
  env' <- mkAriviEnv
  let env = env' { ariviCryptoEnv = CryptoEnv sk
                 , loggerChan = tq
                 -- , udpSocket = sock
                 , udpConnectionHashMap = mutableConnectionHashMap
                 }
  runStdoutLoggingT $ runAppM env (do
                                       let ha = "127.0.0.1"
                                       cid <- openConnection ha 8080 ANT.TCP (generateNodeId rk) ANT.INITIATOR

                                      --  liftIO $ threadDelay 5000
                                      --  sendMessage cid "Hallelujah"
                                      --  sendMessage cid "YOURLORDPROTECTORJESUSCHRIST"
                                      --  liftIO $ print ha
                                       liftIO $ print cid

                                   )
receiver :: SecretKey -> IO ()
receiver sk = do
  print (generateNodeId sk)
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
  runStdoutLoggingT $ runAppM env (
                                       runTCPserver (show (envPort env))
                                  )
main :: IO ()
main = do
  (sender_sk, _) <- generateKeyPair
  (recv_sk, _) <- generateKeyPair
  receiver recv_sk
  sender sender_sk recv_sk

