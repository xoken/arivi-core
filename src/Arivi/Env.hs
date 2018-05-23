module Arivi.Env where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Arivi.Network.Instance
import Control.Monad.IO.Class

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

mkAriviEnv :: AriviEnv
mkAriviEnv = AriviEnv { ariviNetworkInstance = mkAriviNetworkInstance
                      , port = 8080
                      }
