-- |
-- Module: Crypto.Utils.Cipher.AES256
-- This module is provides Encryption and Decryption using AES256 in CTR mode  
module Crypto.Utils.Cipher.AES256
(
    aesEncrypt,
    aesDecrypt,
    nullIV,
    ivAdd,
    IV
)
where 

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine,cipherInit,BlockCipher(..),IV,nullIV,ivAdd)
import Data.ByteArray (convert,ByteArrayAccess,ByteArray)  
import Data.ByteString.Char8 (ByteString)
import Crypto.Error (CryptoFailable(..),CryptoError(..),throwCryptoError)
import Crypto.Utils.Keys.Encryption (SharedSecret)


type PlainText = ByteString
type CipherText = ByteString


sskToAES256Cipher :: SharedSecret -> AES256
sskToAES256Cipher sharedSecretKey = throwCryptoError ((cipherInit (Data.ByteArray.convert sharedSecretKey :: ByteString)) :: CryptoFailable AES256)

aesEncrypt ::  SharedSecret -> IV AES256 -> PlainText -> CipherText
aesEncrypt sharedSecretKey iv plainTextMsg = ctrCombine (sskToAES256Cipher sharedSecretKey) iv plainTextMsg 




aesDecrypt ::  SharedSecret -> IV AES256 -> CipherText -> PlainText
aesDecrypt sharedSecretKey iv plainTextMsg = ctrCombine (sskToAES256Cipher sharedSecretKey) iv plainTextMsg 
