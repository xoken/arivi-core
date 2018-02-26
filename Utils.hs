module Crypto.Utils
(
    getSecretKey,
    getPublicKey,
    sign,
    verify 
) where 


import Crypto.PubKey.Ed25519 (SecretKey,PublicKey,secretKey,toPublic,sign,verify)
import Crypto.Error (throwCryptoError)
import Data.ByteString.Char8 (ByteString)


getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (secretKey seedString)


getPublicKey :: SecretKey -> PublicKey
getPublicKey secretKey = toPublic secretKey