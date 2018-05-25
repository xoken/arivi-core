{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arivi.AppM where

import           Arivi.Env

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Logging
import           Arivi.Network.StreamServer
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Network.Socket
----
import qualified Arivi.Network.Types as ANT
import           Arivi.P2P.Instance
----
import           Arivi.Network.Datagram (createUDPSocket)

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasEnv AppM where
  getEnv = ask

instance HasAriviNetworkInstance AppM where
  getAriviNetworkInstance = ariviNetworkInstance <$> getEnv

instance HasSecretKey AppM where
  getSecretKey = (secretKey . ariviCryptoEnv) <$> getEnv

instance HasLogging AppM where
  getLoggerChan = loggerChan <$> getEnv

instance HasUDPSocket AppM where
  getUDPSocket = udpSocket <$> getEnv

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT




main :: IO ()
main = do
  (sk, _) <- generateKeyPair
  tq <- newTQueueIO :: IO LogChan
  sock <- createUDPSocket "127.0.0.1" (port mkAriviEnv)
  let env = mkAriviEnv {ariviCryptoEnv = CryptoEnv sk, loggerChan = tq,udpSocket = sock}
  runStdoutLoggingT $ runAppM env $ (do
                                        runTCPserver (show (port env))
                                        ha <- liftIO $ inet_addr "127.0.0.1"
                                        cid <- openConnection ha 5000 ANT.TCP "1" ANT.INITIATOR
                                        liftIO $ print cid
                                    )
