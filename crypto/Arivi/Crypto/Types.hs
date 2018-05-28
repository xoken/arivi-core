module Arivi.Crypto.Types (
    CryptoException(..)
) where

import           Control.Exception
import           Crypto.Error      (CryptoError (..))

data CryptoException = CryptoException CryptoError
                     | SignatureVerificationFailed
                     deriving(Eq, Show)

instance Exception CryptoException
