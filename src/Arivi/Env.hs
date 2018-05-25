module Arivi.Env where

import           Arivi.Logging
import           Arivi.Network.Instance
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.Text
import  Network.Socket as Network

data AriviEnv = AriviEnv { ariviNetworkInstance :: AriviNetworkInstance
                         , ariviCryptoEnv :: CryptoEnv
                         , loggerChan :: LogChan
                         , port :: Int                   -- ^ 1) TCP an UDP bind port for new connections
                         , udpSocket ::  Network.Socket  -- ^ UDP server and client Socket for ALL connections
                         }

data CryptoEnv = CryptoEnv { secretKey :: Ed25519.SecretKey
                           }



class (MonadIO m) => HasEnv m where
  getEnv :: m AriviEnv

class (HasEnv m) => HasAriviNetworkInstance m where
  getAriviNetworkInstance :: m AriviNetworkInstance

class (HasEnv m) => HasSecretKey m where
  getSecretKey :: m Ed25519.SecretKey

class (HasEnv m) => HasUDPSocket m where
  getUDPSocket :: m Network.Socket

mkAriviEnv :: AriviEnv
mkAriviEnv = AriviEnv { ariviNetworkInstance = mkAriviNetworkInstance
                      , port = 8080
                      }
