{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Arivi.Env (module Arivi.Env) where

import           Arivi.Logging
import           Arivi.Network.Connection
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import qualified Crypto.PubKey.Ed25519       as Ed25519
import           Data.HashMap.Strict         as HM
import qualified Data.HashTable.IO           as Mutable (CuckooHashTable)
import           Network.Socket              as Network

type HashTable k v = Mutable.CuckooHashTable k v

data AriviEnv = AriviEnv { ariviNetworkInstance :: AriviNetworkInstance
                         , ariviCryptoEnv :: CryptoEnv
                         , loggerChan :: LogChan
                         , envPort :: Int                -- ^ TCP an UDP bind
                                                         --   port for new
                                                         --   connections
                         , udpSocket ::  Network.Socket  -- ^ UDP server and
                                                         --   client Socket for
                                                         --   ALL connections
                         , udpConnectionHashMap :: HashTable ConnectionId
                                                                    Connection
                         }

data CryptoEnv = CryptoEnv { cryptoEnvSercretKey :: Ed25519.SecretKey
                           }



class (MonadIO m, MonadBaseControl IO m) => HasEnv m where
  getEnv :: m AriviEnv

class (HasEnv m) => HasAriviNetworkInstance m where
  getAriviNetworkInstance :: m AriviNetworkInstance

class (HasEnv m) => HasSecretKey m where
  getSecretKey :: m Ed25519.SecretKey

class (HasEnv m) => HasUDPSocket m where
  getUDPSocket :: m Network.Socket

mkAriviEnv :: IO AriviEnv
mkAriviEnv = do
  ani <- mkAriviNetworkInstance
  return AriviEnv { ariviNetworkInstance = ani
                  , envPort = 8080
                  }

data AriviNetworkInstance = AriviNetworkInstance {
  ariviNetworkConnectionMap   :: TVar (HashMap ConnectionId Connection),
  ariviConnectionUpdatesTChan :: TChan (ConnectionId, String)
                                                 }

mkAriviNetworkInstance :: IO AriviNetworkInstance
mkAriviNetworkInstance = do
  tv <- newTVarIO HM.empty
  return AriviNetworkInstance { ariviNetworkConnectionMap = tv }

connectionMap :: AriviNetworkInstance
              -> TVar (HashMap ConnectionId Connection)
connectionMap = ariviNetworkConnectionMap


-- DELETE LATER
mkAriviEnv' :: IO AriviEnv
mkAriviEnv' = do
  ani <- mkAriviNetworkInstance'
  return AriviEnv { ariviNetworkInstance = ani
                  , envPort = 8080
                  }

mkAriviNetworkInstance' :: IO AriviNetworkInstance
mkAriviNetworkInstance' = do
  tv <- newTVarIO HM.empty
  return AriviNetworkInstance { ariviNetworkConnectionMap = tv }
