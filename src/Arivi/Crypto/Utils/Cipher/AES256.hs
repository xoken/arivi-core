-- |
-- Module 	   : Crypto.Utils.Cipher.AES256
-- License     :
-- Maintainer  : Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module is provides Encryption and Decryption using AES256 in CTR mode
--

module Arivi.Crypto.Utils.Cipher.AES256
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
import Arivi.Crypto.Utils.Keys.Encryption (SharedSecret)


type PlainText = ByteString
type CipherText = ByteString


-- | sskToAES256Cipher takes SharedSecret as input and concert's it to AES256
-- type
sskToAES256Cipher :: SharedSecret -> AES256
sskToAES256Cipher sharedSecretKey = throwCryptoError (cipherInit (Data.ByteArray.convert sharedSecretKey :: ByteString) :: CryptoFailable AES256)

-- | aesEncrypt encrypts plainTextMsg using AES256 in CTR mode
aesEncrypt ::  SharedSecret -> IV AES256 -> PlainText -> CipherText
aesEncrypt sharedSecretKey  = ctrCombine (sskToAES256Cipher sharedSecretKey)


-- | aesDecrypt decrypts cipherTextMsg using AES256 in CTR mode
aesDecrypt ::  SharedSecret -> IV AES256 -> CipherText -> PlainText
aesDecrypt sharedSecretKey  = ctrCombine (sskToAES256Cipher sharedSecretKey)
