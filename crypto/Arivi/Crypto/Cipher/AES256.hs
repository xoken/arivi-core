module Arivi.Crypto.Cipher.AES256
    ( aesEncrypt
    , aesDecrypt
    , nullIV
    , ivAdd
    , IV
    , CipherText
    , PlainText
    ) where

import Arivi.Crypto.Utils.Keys.Encryption (SharedSecret)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), IV, cipherInit, ctrCombine, ivAdd, nullIV)
import Crypto.Error (CryptoFailable(..), throwCryptoError)
import Data.ByteArray (convert)
import Data.ByteString.Char8 (ByteString)

-- | PlainText is ByteString
type PlainText = ByteString

-- | PlainText is ByteString
type CipherText = ByteString

-- | sskToAES256Cipher takes SharedSecret as input and concert's it to AES256
-- type
sskToAES256Cipher :: SharedSecret -> AES256
sskToAES256Cipher sharedSecretKey =
    throwCryptoError (cipherInit (Data.ByteArray.convert sharedSecretKey :: ByteString) :: CryptoFailable AES256)

-- | aesEncrypt encrypts plainTextMsg using AES256 in CTR mode
aesEncrypt :: SharedSecret -> IV AES256 -> PlainText -> CipherText
aesEncrypt sharedSecretKey = ctrCombine (sskToAES256Cipher sharedSecretKey)

-- | aesDecrypt decrypts cipherTextMsg using AES256 in CTR mode
aesDecrypt :: SharedSecret -> IV AES256 -> CipherText -> PlainText
aesDecrypt sharedSecretKey = ctrCombine (sskToAES256Cipher sharedSecretKey)
