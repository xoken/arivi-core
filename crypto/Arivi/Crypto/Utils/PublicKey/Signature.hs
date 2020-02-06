module Arivi.Crypto.Utils.PublicKey.Signature
<<<<<<< HEAD
(
    generateKeyPair,
    toByteString,
    getPublicKey,
    getSecretKey
) where

import           Arivi.Crypto.Utils.Random
import           Crypto.Error              (throwCryptoError)
import           Crypto.PubKey.Ed25519     (PublicKey, SecretKey, secretKey,
                                            toPublic)
import           Data.ByteArray            (convert)
import           Data.ByteString.Char8     (ByteString)

=======
    ( generateKeyPair
    , toByteString
    , getPublicKey
    , getSecretKey
    ) where

import Arivi.Crypto.Utils.Random
import Crypto.Error (throwCryptoError)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, secretKey, toPublic)
import Data.ByteArray (convert)
import Data.ByteString.Char8 (ByteString)
>>>>>>> breaking out arivi-core from arivi

-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (Crypto.PubKey.Ed25519.secretKey seedString)

<<<<<<< HEAD

=======
>>>>>>> breaking out arivi-core from arivi
-- | Generates Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey = Crypto.PubKey.Ed25519.toPublic

<<<<<<< HEAD

=======
>>>>>>> breaking out arivi-core from arivi
-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString

generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do
<<<<<<< HEAD
                 randomSeed <- Arivi.Crypto.Utils.Random.getRandomByteString 32
                 let mSecretKey = getSecretKey randomSeed
                 let mPublicKey = getPublicKey mSecretKey
                 return (mSecretKey,mPublicKey)
=======
    randomSeed <- Arivi.Crypto.Utils.Random.getRandomByteString 32
    let mSecretKey = getSecretKey randomSeed
    let mPublicKey = getPublicKey mSecretKey
    return (mSecretKey, mPublicKey)
>>>>>>> breaking out arivi-core from arivi
