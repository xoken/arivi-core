{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Arivi.Env
    ( module Arivi.Env
    ) where

import           Arivi.Network.Connection
import           Arivi.Network.Types      (Parcel (..))
import           Arivi.Utils.Logging
import           Control.Concurrent.STM
import qualified Crypto.PubKey.Ed25519    as Ed25519
import           Data.HashMap.Strict      as HM
import qualified Data.HashTable.IO        as Mutable (CuckooHashTable)
import           Network.Socket           as Network

type HashTable k v = Mutable.CuckooHashTable k v

data AriviEnv = AriviEnv
    { ariviEnvNetworkInstance      :: AriviNetworkInstance
    , ariviEnvCryptoEnv            :: CryptoEnv
    , ariviEnvLoggerChan           :: LogChan
    , ariviEnvPort                 :: Int -- ^ TCP an UDP bind
                                                         --   port for new
                                                         --   connections
    , ariviEnvUdpSocket            :: Network.Socket -- ^ UDP server and
                                                         --   client Socket for
                                                         --   ALL connections
    , ariviEnvUdpConnectionHashMap :: HashTable ConnectionId CompleteConnection
    , ariviEnvDatagramHashMap      :: HashTable ConnectionId (TChan Parcel)
    }

data CryptoEnv = CryptoEnv
    { cryptoEnvSecretKey :: Ed25519.SecretKey
    }

class (Monad m) => HasEnv m where
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
    return AriviEnv {ariviEnvNetworkInstance = ani, ariviEnvPort = 8083}

data AriviNetworkInstance = AriviNetworkInstance
    { ariviNetworkConnectionMap :: TVar (HashMap ConnectionId CompleteConnection)
    , ariviNetworkDatagramMap :: TVar (HashMap ConnectionId (TChan Parcel))
    , ariviConnectionUpdatesTChan :: TChan (ConnectionId, String)
    }

mkAriviNetworkInstance :: IO AriviNetworkInstance
mkAriviNetworkInstance = do
    tv <- newTVarIO HM.empty
    tvDatagram <- newTVarIO HM.empty
    return
        AriviNetworkInstance
        {ariviNetworkConnectionMap = tv, ariviNetworkDatagramMap = tvDatagram}

connectionMap ::
       AriviNetworkInstance -> TVar (HashMap ConnectionId CompleteConnection)
connectionMap = ariviNetworkConnectionMap

datagramMap ::
       AriviNetworkInstance -> TVar (HashMap ConnectionId (TChan Parcel))
datagramMap = ariviNetworkDatagramMap

-- DELETE LATER
mkAriviEnv' :: IO AriviEnv
mkAriviEnv' = do
    ani <- mkAriviNetworkInstance'
    return AriviEnv {ariviEnvNetworkInstance = ani, ariviEnvPort = 8080}

mkAriviNetworkInstance' :: IO AriviNetworkInstance
mkAriviNetworkInstance' = do
    tv <- newTVarIO HM.empty
    return AriviNetworkInstance {ariviNetworkConnectionMap = tv}

-- makeLensesWith camelCaseFields ''AriviNetworkInstance
-- makeLensesWith camelCaseFields ''AriviEnv
-- makeLensesWith camelCaseFields ''CryptoEnv

-- instance Has AriviNetworkInstance AriviEnv where
--     get = networkInstance

-- instance Has SecretKey AriviEnv where
--     get = cryptoEnv . secretKey

-- instance Has Socket AriviEnv where
--    get = udpSocket
