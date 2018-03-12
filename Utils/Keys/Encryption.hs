-- |
-- Module : Crypto.Utils.Keys.Encryption
-- 
-- This module is made for encrypting communications between two parties

module Crypto.Utils.Keys.Encryption
(
    getSecretKey,
    getPublicKey,
    generateKeyPair
) where


import Crypto.Error (throwCryptoError)
import Crypto.PubKey.Curve25519 (SecretKey,PublicKey,secretKey,toPublic)
import Data.ByteString.Char8  (ByteString)
import Data.ByteArray (convert)
import Crypto.Utils.Random



-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = Crypto.Error.throwCryptoError ( Crypto.PubKey.Curve25519.secretKey seedString)


-- | Generatees Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey secretKey =  Crypto.PubKey.Curve25519.toPublic secretKey


-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = ((Data.ByteArray.convert mPublicKey) :: ByteString)



generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do 
                 randomSeed <- (Crypto.Utils.Random.getRandomByteString 32)
                 let secretKey = getSecretKey randomSeed
                 let publicKey = getPublicKey secretKey
                 return (secretKey,publicKey)