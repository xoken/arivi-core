-- |
-- Module : Crypto.Utils.Keys.Signature
-- 
-- This module is made for verifying messages between two parties


module Crypto.Utils.Keys.Signature
(
    getSecretKey,
    getPublicKey,
    sign,
    verify,
<<<<<<< HEAD
    generateKeyPair
=======
    generateKeyPair,
    toHex,
    hexToPublicKey 
>>>>>>> 4ad2acf... Added  function
) where


import Crypto.PubKey.Ed25519 (SecretKey,PublicKey,secretKey,toPublic,sign,verify,publicKey)
import Crypto.Error (throwCryptoError)
import Data.ByteString.Char8 (ByteString)
import Data.ByteArray (convert)
import Data.ByteString.Base16 (encode,decode)
import Crypto.Utils.Random

-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (secretKey seedString)


-- | Generates Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey secretKey = toPublic secretKey


-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = ((Data.ByteArray.convert mPublicKey) :: ByteString)


-- | Converts PublicKey format to Hexadecimal format
toHex :: PublicKey -> ByteString
toHex mPublicKey = (Data.ByteString.Base16.encode (toByteString mPublicKey))

-- | Converts PublicKey from hex form to PublicKey form
hexToPublicKey :: ByteString -> PublicKey
hexToPublicKey hexPublicKey = (Crypto.Error.throwCryptoError (Crypto.PubKey.Ed25519.publicKey (fst (Data.ByteString.Base16.decode hexPublicKey))))

generateKeyPair :: IO (SecretKey, PublicKey)
generateKeyPair = do 
                 randomSeed <- (Crypto.Utils.Random.getRandomByteString 32)
                 let secretKey = getSecretKey randomSeed
                 let publicKey = getPublicKey secretKey
                 return (secretKey,publicKey)