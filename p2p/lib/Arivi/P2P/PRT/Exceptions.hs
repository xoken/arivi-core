module Arivi.P2P.PRT.Exceptions
    ( PRTExecption(..)
    ) where

import           Codec.Serialise   (DeserialiseFailure)
import           Control.Exception
import           Crypto.Error      (CryptoError)

data PRTExecption =
    PeerDeedNotFound
    deriving (Show)

instance Exception PRTExecption
