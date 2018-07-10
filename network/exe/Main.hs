{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (module Main) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Utils.Logging
import           Arivi.Network.Connection           (CompleteConnection,
                                                     ConnectionId)
import           Arivi.Network.Instance
import           Arivi.Network.StreamServer
import qualified Arivi.Network.Types                as ANT (PersonalityType (INITIATOR),
                                                            TransportType (TCP))
import           Control.Concurrent                 (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.HashTable.IO                  as MutableHashMap (new)
-- import           Arivi.Network.Datagram             (createUDPSocket)
import           Control.Exception
import           Data.ByteString.Lazy               as BSL (ByteString)
import           Data.ByteString.Lazy.Char8         as BSLC (pack)
import           Data.Time
import           System.Environment                 (getArgs)

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



sender :: SecretKey -> SecretKey -> Int -> Int -> IO ()
sender sk rk n size = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  mutableConnectionHashMap <- MutableHashMap.new
                                    :: IO (HashTable ConnectionId CompleteConnection)
  env' <- mkAriviEnv
  let env = env' { ariviCryptoEnv = CryptoEnv sk
                 , loggerChan = tq
                 -- , udpSocket = sock
                 , udpConnectionHashMap = mutableConnectionHashMap
                 }
  runStdoutLoggingT $ runAppM env (do

                                       let ha = "127.0.0.1"
                                       cidOrFail <- openConnection ha 8083 ANT.TCP (generateNodeId rk) ANT.INITIATOR
                                       case cidOrFail of
                                          Left e -> throw e
                                          Right cid -> do
                                            time <- liftIO  getCurrentTime
                                            liftIO $ print time
                                            mapM_ (\_ -> sendMessage cid (a size)) [1..n]
                                            time2 <- liftIO getCurrentTime
                                            liftIO $ print time2
                                       liftIO $ print "done"
                                   )
receiver :: SecretKey -> IO ()
receiver sk = do
  tq <- newTQueueIO :: IO LogChan
  -- sock <- createUDPSocket "127.0.0.1" (envPort mkAriviEnv)
  mutableConnectionHashMap1 <- MutableHashMap.new
                                    :: IO (HashTable ConnectionId CompleteConnection)
  env' <- mkAriviEnv
  let env = env' { ariviCryptoEnv = CryptoEnv sk
                 , loggerChan = tq
                 -- , udpSocket = sock
                 , udpConnectionHashMap = mutableConnectionHashMap1
                 }
  runStdoutLoggingT $ runAppM env (do
                                       runTCPServer (show (envPort env))
                                       (newConn, _) <- getNewConnection
                                       liftIO $ print newConn
                                       msg <- readMessage newConn
                                       liftIO $ print msg
                                  )

main :: IO ()
main = do
  [size, n] <- getArgs
  (sender_sk, _) <- generateKeyPair
  (recv_sk, _) <- generateKeyPair
  _ <- receiver recv_sk `concurrently` (threadDelay 1000000 >> sender sender_sk recv_sk (read n) (read size))
  threadDelay 1000000000000
  return ()

a :: Int -> BSL.ByteString
a n = BSLC.pack (replicate n 'a')
