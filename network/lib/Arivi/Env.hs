<<<<<<< HEAD
{-# LANGUAGE FlexibleContexts  #-}
=======
{-# LANGUAGE FlexibleContexts #-}
>>>>>>> breaking out arivi-core from arivi
{-# LANGUAGE FlexibleInstances #-}

module Arivi.Env
    ( module Arivi.Env
    ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

<<<<<<< HEAD
data AriviEnv = AriviEnv
    { ariviEnvCryptoEnv :: CryptoEnv
    , ariviEnvTcpPort   :: Int -- ^ TCP port for new connections
    , ariviEnvUdpPort   :: Int -- ^ UDP port for new connections
    }

data CryptoEnv = CryptoEnv
    { cryptoEnvSecretKey :: Ed25519.SecretKey
    }
=======
data AriviEnv =
    AriviEnv
        { ariviEnvCryptoEnv :: CryptoEnv
        , ariviEnvTcpPort :: Int -- ^ TCP port for new connections
        , ariviEnvUdpPort :: Int -- ^ UDP port for new connections
        }

data CryptoEnv =
    CryptoEnv
        { cryptoEnvSecretKey :: Ed25519.SecretKey
        }
>>>>>>> breaking out arivi-core from arivi

class (Monad m) =>
      HasNetworkEnv m
    where
    getEnv :: m AriviEnv

class (HasNetworkEnv m) =>
      HasSecretKey m
    where
    getSecretKey :: m Ed25519.SecretKey
    getSecretKey = cryptoEnvSecretKey . ariviEnvCryptoEnv <$> getEnv

mkAriviEnv :: Int -> Int -> Ed25519.SecretKey -> AriviEnv
mkAriviEnv tcpPort udpPort sk =
<<<<<<< HEAD
    AriviEnv
        { ariviEnvCryptoEnv = CryptoEnv sk
        , ariviEnvTcpPort = tcpPort
        , ariviEnvUdpPort = udpPort
        }
-- makeLensesWith camelCaseFields ''AriviNetworkInstance
-- makeLensesWith camelCaseFields ''AriviEnv
-- makeLensesWith camelCaseFields ''CryptoEnv
-- instance Has AriviNetworkInstance AriviEnv where
--     get = networkInstance
-- instance Has SecretKey AriviEnv where
--     get = cryptoEnv . secretKey
-- instance Has Socket AriviEnv where
--    get = udpSocket
=======
    AriviEnv {ariviEnvCryptoEnv = CryptoEnv sk, ariviEnvTcpPort = tcpPort, ariviEnvUdpPort = udpPort}
>>>>>>> breaking out arivi-core from arivi
--
