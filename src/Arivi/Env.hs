module Arivi.Env where

import           Arivi.Logging
import           Arivi.Network.Instance
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.Text

data AriviEnv = AriviEnv { ariviNetworkInstance :: AriviNetworkInstance
                         , ariviCryptoEnv :: CryptoEnv
                         , loggerChan :: LogChan
                         , port :: Int
                         }

data CryptoEnv = CryptoEnv { secretKey :: Ed25519.SecretKey
                           }



class (MonadIO m) => HasEnv m where
  getEnv :: m AriviEnv

class (HasEnv m) => HasAriviNetworkInstance m where
  getAriviNetworkInstance :: m AriviNetworkInstance

class (HasEnv m) => HasSecretKey m where
  getSecretKey :: m Ed25519.SecretKey

mkAriviEnv :: AriviEnv
mkAriviEnv = AriviEnv { ariviNetworkInstance = mkAriviNetworkInstance
                      , port = 8080
                      }
