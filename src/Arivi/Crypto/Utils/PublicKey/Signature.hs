module Arivi.Crypto.Utils.PublicKey.Signature
(
    generateKeyPair,
    toByteString,
    getPublicKey
) where

import           Arivi.Crypto.Utils.Random
import           Crypto.Error              (CryptoFailable, throwCryptoError)
import           Crypto.PubKey.Ed25519     (PublicKey, SecretKey, Signature, publicKey,
                                            secretKey, toPublic, sign)
import           Data.ByteArray            (convert)
import           Data.ByteString.Char8     (ByteString)
import           Data.Proxy


-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (Crypto.PubKey.Ed25519.secretKey seedString)


-- | Generates Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey = Crypto.PubKey.Ed25519.toPublic


-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString

generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do
                 randomSeed <- Arivi.Crypto.Utils.Random.getRandomByteString 32
                 let secretKey = getSecretKey randomSeed
                 let publicKey = getPublicKey secretKey
                 return (secretKey,publicKey)



