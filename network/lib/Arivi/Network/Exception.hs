{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Arivi.Network.Exception
    ( AriviNetworkException(..)
    , mapIOException
    ) where

<<<<<<< HEAD
import           Codec.Serialise   (DeserialiseFailure (..))
import           Control.Exception
import           Crypto.Error      (CryptoError (..))
=======
--import Codec.CBOR.Read hiding (DeserialiseFailure)
import Codec.Serialise (DeserialiseFailure(..))
import Control.Exception
import Crypto.Error (CryptoError(..))
>>>>>>> breaking out arivi-core from arivi

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
<<<<<<< HEAD
        Left e  -> throw (f e)
=======
        Left e -> throw (f e)
>>>>>>> breaking out arivi-core from arivi
        Right r -> return r
