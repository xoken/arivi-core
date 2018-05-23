{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arivi.AppM where

import           Arivi.Env

import           Arivi.Crypto.Utils.Keys.Signature

import           Network.Socket
import           Arivi.Network.StreamServer
import           Control.Monad.Reader

----
import qualified Arivi.Network.Types as ANT
import           Arivi.P2P.Instance
----
-- class HasLogger m where

type AppM = ReaderT AriviEnv IO

instance HasEnv AppM where
  getEnv = ask

instance HasAriviNetworkInstance AppM where
  getAriviNetworkInstance = ariviNetworkInstance <$> getEnv

instance HasSecretKey AppM where
  getSecretKey = (secretKey . ariviCryptoEnv) <$> getEnv


runAppM :: AriviEnv -> AppM a -> IO a
runAppM env app = runReaderT app env


main :: IO ()
main = do
  (sk, _) <- generateKeyPair
  let env = mkAriviEnv {ariviCryptoEnv = CryptoEnv sk}
  runAppM env $ (do
                    runTCPserver (show (port env))
                    ha <- liftIO $ inet_addr "127.0.0.1"
                    cid <- openConnection ha 5000 ANT.TCP "1" ANT.INITIATOR
                    liftIO $ print cid
                )
