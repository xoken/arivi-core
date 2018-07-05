module Arivi.Network.Exception
(
      AriviNetworkException(..)
    , mapIOException
) where

import           Codec.Serialise   (DeserialiseFailure)
import           Control.Exception
import           Crypto.Error      (CryptoError)

data AriviNetworkException
    = NetworkCryptoException CryptoError
    | NetworkSignatureVerificationFailedException
    | NetworkSocketException
    | NetworkWrongParcelException
    | NetworkTimeoutException
    | NetworkDeserialiseException DeserialiseFailure
    deriving (Show)

instance Exception AriviNetworkException

mapIOException :: (Exception e) => (SomeException -> e) -> IO a -> IO a
mapIOException f ioa = do
    aOrFail <- try ioa
    case aOrFail of
        Left e  -> throw (f e)
        Right r -> return r
