-- |
-- Module      : Arivi.Crypto.Utils.Keys.Encryption
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module is made for encrypting communications between two parties
--
--
--
--  This is ECIES implementation using Elliptic Curve Diffie Hellman key exchange
-- inspired from Crypto.PubKey.ECIES
-- <https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-ECIES.html>
-- (why not use Crypto.PubKey.ECIES itself?. Since we are using randomByteString
-- generation from Raaz Library for key generations)
--
--  Sender will compute ephemeral Key Pairs. He uses remotePublicKey and his
-- computed ephemeralPrivateKey to compute SharedSecret for further
-- communications, then he encrypts  ephemeralPublicKey using remotePublicKey
-- and sends to remote. Now remote will decrypt received ephemeralPublicKey
-- using his secretKey and uses this ephemeralPublicKey and his secretKey to
-- get the SharedSecret (User has to take care of ephemeral Public Key encryption
-- and decryption)
--

module Arivi.Crypto.Utils.Keys.Encryption
(
      SharedSecret
    , PublicKey
    , SecretKey
    , createSharedSecretKey
    , derivedSharedSecretKey
    , generateKeyPair
    , getPublicKey
    , getSecretKey
    , publicKey
    , throwCryptoError
    , toByteString
) where


import           Crypto.ECC                (Curve_X25519, SharedSecret, ecdh)
import           Crypto.Error              (throwCryptoError, CryptoFailable)
import           Crypto.PubKey.Curve25519  (PublicKey, SecretKey, publicKey,
                                            secretKey, toPublic)
import           Data.ByteArray            (convert)
import           Data.ByteString.Char8     (ByteString)
import           Data.Proxy

import           Arivi.Crypto.Utils.Random



-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = Crypto.Error.throwCryptoError (Crypto.PubKey.Curve25519.secretKey seedString)


-- | Generates Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey =  Crypto.PubKey.Curve25519.toPublic


-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = Data.ByteArray.convert mPublicKey :: ByteString


-- | This function generates (SecretKey,PublicKey) pair using Raaz's Random Seed
-- generation
generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do
                 randomSeed <-  Arivi.Crypto.Utils.Random.getRandomByteString 32
                 let mSecretKey = getSecretKey randomSeed
                 let mPublicKey = getPublicKey mSecretKey
                 return (mSecretKey,mPublicKey)






-- | This is Elliptic curve. user of this library don't have to worry about this
curveX25519 :: Proxy Curve_X25519
curveX25519 = Proxy :: Proxy Curve_X25519

-- | Using createSharedSecreatKey sender will create SharedSecret for himself
-- and shares encrypted ephemeralPublicKey with remote

createSharedSecretKey :: SecretKey -> PublicKey -> CryptoFailable SharedSecret
createSharedSecretKey = ecdh curveX25519



-- | Remote will decrypt received SharedSecret with his secretKey and gets
-- ephemeralPublicKey and computes SecretKey using derivedSharedSecreatKey
-- function

derivedSharedSecretKey :: PublicKey -> SecretKey -> CryptoFailable SharedSecret
derivedSharedSecretKey ephemeralPublicKey remotePrivateKey =  ecdh curveX25519 remotePrivateKey ephemeralPublicKey
