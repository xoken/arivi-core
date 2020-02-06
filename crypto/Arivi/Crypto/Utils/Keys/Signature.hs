<<<<<<< HEAD
-- |
-- Module      : Arivi.Crypto.Utils.Keys.Signature
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability
--
-- This module is made for verifying messages between two parties
--

module Arivi.Crypto.Utils.Keys.Signature
(
      PublicKey
=======
--
-- This module is made for verifying messages between two parties
--
module Arivi.Crypto.Utils.Keys.Signature
    ( PublicKey
>>>>>>> breaking out arivi-core from arivi
    , SecretKey
    , getSecretKey
    , getPublicKey
    , generateKeyPair
    , hexToPublicKey
    , hexToSecretKey
    , publicKeytoHex
    , secretKeyToHex
    , sign
    , toByteString
    , verify
<<<<<<< HEAD
) where


import           Arivi.Crypto.Utils.Random
import           Crypto.Error              (throwCryptoError)
import           Crypto.PubKey.Ed25519     (PublicKey, SecretKey, publicKey,
                                            secretKey, sign, toPublic, verify)
import           Data.ByteArray            (convert)
import           Data.ByteString.Base16    (decode, encode)
import           Data.ByteString.Char8     (ByteString)
=======
    ) where

import Arivi.Crypto.Utils.Random
import Crypto.Error (throwCryptoError)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, publicKey, secretKey, sign, toPublic, verify)
import Data.ByteArray (convert)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Char8 (ByteString)
>>>>>>> breaking out arivi-core from arivi

-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (secretKey seedString)

<<<<<<< HEAD

=======
>>>>>>> breaking out arivi-core from arivi
-- | Generates Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey = toPublic

<<<<<<< HEAD

=======
>>>>>>> breaking out arivi-core from arivi
-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString

<<<<<<< HEAD

=======
>>>>>>> breaking out arivi-core from arivi
-- | Converts PublicKey format to Hexadecimal format
publicKeytoHex :: PublicKey -> ByteString
publicKeytoHex mPublicKey = Data.ByteString.Base16.encode (toByteString mPublicKey)

-- | Converts PublicKey from hex form to PublicKey form
hexToPublicKey :: ByteString -> PublicKey
<<<<<<< HEAD
hexToPublicKey hexPublicKey = Crypto.Error.throwCryptoError (Crypto.PubKey.Ed25519.publicKey (fst (Data.ByteString.Base16.decode hexPublicKey)))

=======
hexToPublicKey hexPublicKey =
    Crypto.Error.throwCryptoError (Crypto.PubKey.Ed25519.publicKey (fst (Data.ByteString.Base16.decode hexPublicKey)))
>>>>>>> breaking out arivi-core from arivi

-- | This function generates (SecretKey,PublicKey) pair using Raaz's Random Seed
-- generation
generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do
<<<<<<< HEAD
                 randomSeed <- Arivi.Crypto.Utils.Random.getRandomByteString 32
                 let mSecretKey = getSecretKey randomSeed
                 let mPublicKey = getPublicKey mSecretKey
                 return (mSecretKey,mPublicKey)


-- | This function is used for converting Secret Key from SecretKey form to hex form
secretKeyToHex :: SecretKey-> ByteString
secretKeyToHex mSecretKey= Data.ByteString.Base16.encode (Data.ByteArray.convert mSecretKey ::ByteString)


-- | This function is used for converting Secret Key from hex form to SecretKey form
hexToSecretKey :: ByteString -> SecretKey
hexToSecretKey hexSecretKey = Crypto.Error.throwCryptoError (Crypto.PubKey.Ed25519.secretKey (fst (Data.ByteString.Base16.decode hexSecretKey)))
=======
    randomSeed <- Arivi.Crypto.Utils.Random.getRandomByteString 32
    let mSecretKey = getSecretKey randomSeed
    let mPublicKey = getPublicKey mSecretKey
    return (mSecretKey, mPublicKey)

-- | This function is used for converting Secret Key from SecretKey form to hex form
secretKeyToHex :: SecretKey -> ByteString
secretKeyToHex mSecretKey = Data.ByteString.Base16.encode (Data.ByteArray.convert mSecretKey :: ByteString)

-- | This function is used for converting Secret Key from hex form to SecretKey form
hexToSecretKey :: ByteString -> SecretKey
hexToSecretKey hexSecretKey =
    Crypto.Error.throwCryptoError (Crypto.PubKey.Ed25519.secretKey (fst (Data.ByteString.Base16.decode hexSecretKey)))
>>>>>>> breaking out arivi-core from arivi
