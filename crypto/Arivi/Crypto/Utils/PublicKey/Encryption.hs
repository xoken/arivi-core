
module Arivi.Crypto.Utils.PublicKey.Encryption
(
    generateKeyPair,
    getSecretKey,
    getPublicKey,
    toByteString,
    createSharedSecretKey,
    derivedSharedSecretKey,
    sharedSecretToByteString
) where


import           Crypto.ECC               (Curve_X25519, SharedSecret, ecdh)
import           Crypto.Error             (CryptoFailable, throwCryptoError)
import qualified Crypto.PubKey.Curve25519 as Curve25519
import           Data.ByteArray           (convert)
import           Data.ByteString.Char8    (ByteString)
import           Data.Proxy

import           Crypto.Hash              (Digest, SHA256, hash)
import qualified Crypto.PubKey.Ed25519    as Ed25519

sha256 :: ByteString -> Digest SHA256
sha256 = hash

-- | The function used to convert the signing secret key to encryption secret key
encryptionSecretDerivationFunction :: ByteString -> Digest SHA256
encryptionSecretDerivationFunction = sha256

-- | Generate secret key for encryption using secret key of signing
getSecretKey :: Ed25519.SecretKey -> Curve25519.SecretKey
getSecretKey signingSecretKey = throwCryptoError secretKey
    where
        bsSecretKey = encryptionSecretDerivationFunction (Data.ByteArray.convert signingSecretKey :: ByteString)
        secretKey = Curve25519.secretKey bsSecretKey

-- | Get curve25519 public key for corresponding sk
getPublicKey :: Curve25519.SecretKey -> Curve25519.PublicKey
getPublicKey = Curve25519.toPublic

-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: Curve25519.PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString

-- | Generate an encryption keypair
generateKeyPair :: Ed25519.SecretKey -> IO(Curve25519.SecretKey, Curve25519.PublicKey)
generateKeyPair sk = do
    let encryptSK = getSecretKey sk
    let encryptPK = getPublicKey encryptSK
    return (encryptSK, encryptPK)

curveX25519 :: Proxy Curve_X25519
curveX25519 = Proxy :: Proxy Curve_X25519

-- | Using createSharedSecreatKey sender will create SharedSecret for himself
-- and shares encrypted ephemeralPublicKey with remote

createSharedSecretKey :: Curve25519.SecretKey -> Curve25519.PublicKey ->  CryptoFailable Crypto.ECC.SharedSecret
createSharedSecretKey = ecdh curveX25519

-- | Convert a shared secret to bytestring
sharedSecretToByteString :: SharedSecret -> ByteString
sharedSecretToByteString secret = Data.ByteArray.convert secret :: ByteString

-- | Remote will decrypt received SharedSecret with his secretKey and gets
-- ephemeralPublicKey and computes SecretKey using derivedSharedSecreatKey
-- function

derivedSharedSecretKey :: Curve25519.PublicKey -> Curve25519.SecretKey -> CryptoFailable Crypto.ECC.SharedSecret
derivedSharedSecretKey ephemeralPublicKey remotePrivateKey =  ecdh curveX25519 remotePrivateKey ephemeralPublicKey
