module Arivi.Env where

import           Arivi.Network.Instance
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Catch
import qualified Crypto.PubKey.Ed25519 as Ed25519

data AriviEnv = AriviEnv { ariviNetworkInstance :: AriviNetworkInstance
                         , ariviCryptoEnv :: CryptoEnv
                         , port :: Int
                         }

data CryptoEnv = CryptoEnv { secretKey :: Ed25519.SecretKey
                           }



class (MonadIO m, Monad m) => HasEnv m where
  getEnv :: m AriviEnv

class (HasEnv m) => HasAriviNetworkInstance m where
  getAriviNetworkInstance :: m AriviNetworkInstance

class (HasEnv m) => HasSecretKey m where
  getSecretKey :: m Ed25519.SecretKey

class (MonadLogger m, HasEnv m, MonadThrow m, MonadCatch m) => HasLogging m where

mkAriviEnv :: AriviEnv
mkAriviEnv = AriviEnv { ariviNetworkInstance = mkAriviNetworkInstance
                      , port = 8080
                      }
