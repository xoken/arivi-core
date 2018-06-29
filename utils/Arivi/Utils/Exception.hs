module Arivi.Utils.Exception
    ( AriviException(..)
    , mapIOException
    ) where

import           Codec.Serialise   (DeserialiseFailure)
import           Control.Exception
import           Crypto.Error      (CryptoError (..))

data AriviException
    = AriviDeserialiseException DeserialiseFailure
    | AriviCryptoException CryptoError
    | AriviSignatureVerificationFailedException
    | AriviSocketException
    | AriviWrongParcelException
    | AriviTimeoutException
    | AriviInvalidConnectionIdException
    | KademliaKbIndexDoesNotExist
    | KademliaInvalidPeer
    | KademliaDefaultPeerDoesNotExists
    | HandlerSendMessageTimeout
    | HandlerOpenConnectionError
    | HandlerNotRequest
    | HandlerConnectionBroken
    | KademliaInvalidRequest
    | KademliaInvalidResponse
    deriving (Show)

instance Exception AriviException

mapIOException :: (Exception e) => (SomeException -> e) -> IO a -> IO a
mapIOException f ioa = do
    aOrFail <- try ioa
    case aOrFail of
        Left e  -> throw (f e)
        Right r -> return r
