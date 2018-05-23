{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arivi.AppM where

import           Arivi.Env

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Network.StreamServer
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Network.Socket

----
import qualified Arivi.Network.Types as ANT
import           Arivi.P2P.Instance
----
-- class HasLogger m where

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasEnv AppM where
  getEnv = ask

instance HasAriviNetworkInstance AppM where
  getAriviNetworkInstance = ariviNetworkInstance <$> getEnv

instance HasSecretKey AppM where
  getSecretKey = (secretKey . ariviCryptoEnv) <$> getEnv

instance HasLogging AppM where


runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT


main :: IO ()
main = do
  (sk, _) <- generateKeyPair
  let env = mkAriviEnv {ariviCryptoEnv = CryptoEnv sk}
  runStdoutLoggingT $ runAppM env $ (do
                                        runTCPserver (show (port env))
                                        ha <- liftIO $ inet_addr "127.0.0.1"
                                        cid <- openConnection ha 5000 ANT.TCP "1" ANT.INITIATOR
                                        liftIO $ print cid
                                    )
