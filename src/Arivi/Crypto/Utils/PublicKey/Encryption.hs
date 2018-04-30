module Arivi.Crypto.Utils.PublicKey.Encryption
(
	generateKeyPair, 
	toByteString
) where


import           Crypto.ECC                (Curve_X25519, SharedSecret, ecdh)
import           Crypto.Error              (CryptoFailable, throwCryptoError)
import qualified Crypto.PubKey.Curve25519  (PublicKey, SecretKey, publicKey,
                                            secretKey, toPublic)
import           Data.ByteArray            (convert)
import           Data.ByteString.Char8     (ByteString)
import           Data.Proxy

import           Arivi.Crypto.Utils.Random
import qualified Crypto.PubKey.Ed25519     (PublicKey, SecretKey)
import           Crypto.Hash               (SHA256, Digest, hash)
import qualified Arivi.Crypto.Utils.PublicKey.Signature

sha256 :: ByteString -> Digest SHA256
sha256 = hash

-- | The function used to convert the signing secret key to encryption secret key
encryptionSecretDerivationFunction = sha256

-- | Generate secret key for encryption using secret key of signing
generateSecretKey :: Crypto.PubKey.Ed25519.SecretKey -> Crypto.PubKey.Curve25519.SecretKey

generateSecretKey signingSecretKey = throwCryptoError secretKey 
	where
		bsSecretKey = encryptionSecretDerivationFunction (Data.ByteArray.convert signingSecretKey :: ByteString)
		secretKey = Crypto.PubKey.Curve25519.secretKey bsSecretKey

-- | Get curve25519 public key for corresponding sk
getPublicKey :: Crypto.PubKey.Curve25519.SecretKey -> Crypto.PubKey.Curve25519.PublicKey
getPublicKey = Crypto.PubKey.Curve25519.toPublic

-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: Crypto.PubKey.Curve25519.PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString

-- | Generate an encryption keypair
generateKeyPair :: Crypto.PubKey.Ed25519.SecretKey -> IO(Crypto.PubKey.Curve25519.SecretKey, Crypto.PubKey.Curve25519.PublicKey)

generateKeyPair sk = do
	let encryptSK = generateSecretKey sk
	let encryptPK = getPublicKey encryptSK
	return (encryptSK, encryptPK)

-- | This is Elliptic curve. user of this library don't have to worry about this

curveX25519 = Proxy :: Proxy Curve_X25519

-- | Using createSharedSecreatKey sender will create SharedSecret for himself
-- and shares encrypted ephemeralPublicKey with remote

createSharedSecretKey :: Crypto.PubKey.Curve25519.SecretKey -> Crypto.PubKey.Curve25519.PublicKey ->  Crypto.ECC.SharedSecret
createSharedSecretKey = ecdh curveX25519



-- | Remote will decrypt received SharedSecret with his secretKey and gets
-- ephemeralPublicKey and computes SecretKey using derivedSharedSecreatKey
-- function

derivedSharedSecretKey :: Crypto.PubKey.Curve25519.PublicKey -> Crypto.PubKey.Curve25519.SecretKey -> Crypto.ECC.SharedSecret
derivedSharedSecretKey ephemeralPublicKey remotePrivateKey =  ecdh curveX25519 remotePrivateKey ephemeralPublicKey
