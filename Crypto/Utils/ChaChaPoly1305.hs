-- |
-- Module : Crypto.Utils.ChaChaPoly1305 
-- 
-- This module is made for encrypting communications between two parties
-- using ChaCha20 algorithm and validation of message integrity, authenticity 
-- using Poly1305


module Crypto.Utils.ChaChaPoly1305 
(
    chachaEncrypt,
    chachaDecrypt,
    getCipherTextAuthPair,
) where 

import Data.ByteString.Char8 as B 
import Data.ByteArray
import Crypto.Error
import Crypto.Cipher.ChaChaPoly1305 as ChachaPoly1305


getCipherTextAuthPair :: ByteString -> (ByteString, ByteString)
getCipherTextAuthPair mCipherText = (B.reverse (B.drop 16 (B.reverse mCipherText)), B.reverse (B.take 16 (B.reverse mCipherText)))





-- | Sender encrypts plain text using shared secret key
-- and calculated 12 Byte Nonce, appends Poly1305 authentication tag end of cipher text

chachaEncrypt :: (ByteArrayAccess iv, ByteArrayAccess key, ByteArrayAccess header) => iv -> key -> header -> ByteString -> CryptoFailable ByteString
chachaEncrypt nonce key header plaintext = do
    initialState <-ChachaPoly1305.nonce12 nonce >>= ChachaPoly1305.initialize key
    let
        stateOne = ChachaPoly1305.finalizeAAD $ ChachaPoly1305.appendAAD header initialState
        (out, stateTwo) = ChachaPoly1305.encrypt plaintext stateOne
        auth = ChachaPoly1305.finalize stateTwo
    return $ out `B.append` Data.ByteArray.convert auth


-- | Receiver separates cipher text and authentication tag using `getCipherTextAuthPair` 
-- and decrypts cipher text using 12 Byte Nonce and shared secret key, if 
-- authentication tag is valid


chachaDecrypt :: (ByteArrayAccess iv, ByteArrayAccess key, ByteArrayAccess header, ByteArray authenticationTag, ByteArray cipherTextMessage) 
    => iv -> key -> header -> authenticationTag -> cipherTextMessage -> CryptoFailable ByteString

chachaDecrypt nonce key header auth cipherText = do
    initialState <-ChachaPoly1305.nonce12 nonce >>= ChachaPoly1305.initialize key

    let
        stateOne = ChachaPoly1305.finalizeAAD $ ChachaPoly1305.appendAAD header initialState
        (out, stateTwo) = ChachaPoly1305.decrypt cipherText stateOne
        authCalc = ChachaPoly1305.finalize stateTwo
    if auth == (Data.ByteArray.convert authCalc)
        then return  (Data.ByteArray.convert out)
        else CryptoFailed Crypto.Error.CryptoError_AuthenticationTagSizeInvalid