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
import Data.ByteArray (convert)
import Data.ByteString.Base16 (encode)


-- | Takes a 32 bytes seed and produces SecretKey
getSecretKey :: ByteString -> SecretKey
getSecretKey seedString = throwCryptoError (secretKey seedString)


-- | Generatees Public Key using the given Secret Key
getPublicKey :: SecretKey -> PublicKey
getPublicKey secretKey = toPublic secretKey


-- | Takes PublicKey as input and extracts the string part of PublicKey
toByteString :: PublicKey -> ByteString
toByteString mPublicKey = ((Data.ByteArray.convert mPublicKey) :: ByteString)


-- | Converts PublicKey format to Hexadecimal format
toHex :: PublicKey -> ByteString
toHex mPublicKey = (Data.ByteString.Base16.encode (toByteString mPublicKey))