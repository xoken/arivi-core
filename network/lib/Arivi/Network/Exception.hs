{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Arivi.Network.Exception
    ( AriviNetworkException(..)
    , mapIOException
    ) where

import           Codec.Serialise   (DeserialiseFailure (..))
import           Control.Exception
import           Crypto.Error      (CryptoError (..))

data AriviNetworkException
    = NetworkCryptoException CryptoError
    | NetworkSignatureVerificationFailedException
    | NetworkSocketException
    | NetworkWrongParcelException
    | NetworkTimeoutException
    | NetworkDeserialiseException DeserialiseFailure
    | ReplayAttackException
    deriving (Eq, Ord, Show)

deriving instance Eq DeserialiseFailure

deriving instance Ord DeserialiseFailure

deriving instance Ord CryptoError

instance Exception AriviNetworkException

mapIOException :: (Exception e) => (SomeException -> e) -> IO a -> IO a
mapIOException f ioa = do
    aOrFail <- try ioa
    case aOrFail of
        Left e  -> throw (f e)
        Right r -> return r
