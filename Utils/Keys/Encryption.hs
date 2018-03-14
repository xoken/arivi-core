-- |
-- Module : Crypto.Utils.Keys.Encryption
-- 
-- This module is made for encrypting communications between two parties



-- | This is ECIES implementation using Elliptic Curve Diffie Hellman key exchange
-- inspired from Crypto.PubKey.ECIES <https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-ECIES.html>  
-- (why not use Crypto.PubKey.ECIES itself?. Since we are using randomByteString 
-- generation from Raaz Library for key generations)




-- | Sender will compute ephemeral Key Pairs. He uses remotePublicKey and his 
-- computed ephemeralPrivateKey to compute SharedSecret for further  
-- communications, then he encrypts  ephemeralPublicKey using remotePublicKey
-- and sends to remote. Now remote will decrypt received ephemeralPublicKey 
-- using his secretKey and uses this ephemeralPublicKey and his secretKey to 
-- get the SharedSecret (User has to take care of ephemeral Public Key encryption
-- and decryption)




module Crypto.Utils.Keys.Encryption
(
    getSecretKey,
    getPublicKey,
    generateKeyPair,
    createSharedSecreatKey,
    derivedSharedSecreatKey

) where


import Crypto.Error (throwCryptoError,CryptoFailable)
import Crypto.PubKey.Curve25519 (SecretKey,PublicKey,secretKey,toPublic)
import Data.ByteString.Char8  (ByteString)
import Data.ByteArray (convert)
import Crypto.ECC (ecdh,Curve_X25519,SharedSecret)
import Data.Proxy

import Crypto.Utils.Random



-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = Crypto.Error.throwCryptoError (Crypto.PubKey.Curve25519.secretKey seedString)


-- | Generates Public Key using the given Secret Key
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






-- | This is Elliptic curve. user of this library don't have to worry about this

curveX25519 = Proxy :: Proxy Curve_X25519

-- | Using createSharedSecreatKey sender will create SharedSecret for himself
-- and shares encrypted ephemeralPublicKey with remote

createSharedSecreatKey :: SecretKey -> PublicKey -> CryptoFailable Crypto.ECC.SharedSecret
createSharedSecreatKey ephemeralPrivateKey remotePublicKey = ecdh curveX25519 ephemeralPrivateKey remotePublicKey



-- | Remote will decrypt received SharedSecret with his secretKey and gets 
-- ephemeralPublicKey and computes SecretKey using derivedSharedSecreatKey 
-- function

derivedSharedSecreatKey :: PublicKey -> SecretKey -> CryptoFailable Crypto.ECC.SharedSecret
derivedSharedSecreatKey ephemeralPublicKey remotePrivateKey = ecdh curveX25519 remotePrivateKey ephemeralPublicKey