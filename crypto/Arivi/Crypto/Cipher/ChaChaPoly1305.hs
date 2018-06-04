-- |
-- Module      : Arivi.Crypto.Cipher.ChaChaPoly1305
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module is made for encrypting communications between two parties
-- using ChaCha20 algorithm and validation of message integrity, authenticity
-- using Poly1305
--

module Arivi.Crypto.Cipher.ChaChaPoly1305
(
    chachaEncrypt,
    chachaDecrypt,
    getCipherTextAuthPair,
) where

import           Crypto.Cipher.ChaChaPoly1305 as ChachaPoly1305
import           Crypto.Error
import           Data.ByteArray
import           Data.ByteString.Char8        as B

-- | This separates the cipherText and authentication part and returns
--   (cipherText,Auth) pair last 16 bytes of received message is Auth Part
getCipherTextAuthPair :: ByteString -> (ByteString, ByteString)
getCipherTextAuthPair mCipherText = B.splitAt
                                        (B.length mCipherText - 16) mCipherText





-- | Sender encrypts plain text using shared secret key
-- and calculated 12 Byte Nonce, appends Poly1305 authentication tag end of cipher text

chachaEncrypt :: (ByteArrayAccess key,
                  ByteArrayAccess header)
                  => ByteString
                  -> key
                  -> header
                  -> ByteString
                  -> CryptoFailable ByteString
chachaEncrypt nonce key header plaintext = do
    -- Is choosing a non random constVal secure?
    let constVal = B.pack "\NUL\NUL\NUL\NUL"
    initialState <- ChachaPoly1305.nonce8 constVal nonce >>= ChachaPoly1305.initialize key
    let
        stateOne = ChachaPoly1305.finalizeAAD $ ChachaPoly1305.appendAAD header initialState
        (out, stateTwo) = ChachaPoly1305.encrypt plaintext stateOne
        auth = ChachaPoly1305.finalize stateTwo
    return $ out `B.append` Data.ByteArray.convert auth


-- | Receiver separates cipher text and authentication tag using `getCipherTextAuthPair`
-- and decrypts cipher text using 12 Byte Nonce and shared secret key, if
-- authentication tag is valid


chachaDecrypt :: (ByteArrayAccess key,
                  ByteArrayAccess header,
                  ByteArray authenticationTag,
                  ByteArray cipherTextMessage)
                  => ByteString
                  -> key
                  -> header
                  -> authenticationTag
                  -> cipherTextMessage
                  -> CryptoFailable ByteString

chachaDecrypt nonce key header auth cipherText = do
    let constVal = B.pack "\NUL\NUL\NUL\NUL"
    initialState <-ChachaPoly1305.nonce8 constVal nonce >>= ChachaPoly1305.initialize key

    let
        stateOne = ChachaPoly1305.finalizeAAD $ ChachaPoly1305.appendAAD header initialState
        (out, stateTwo) = ChachaPoly1305.decrypt cipherText stateOne
        authCalc = ChachaPoly1305.finalize stateTwo
    if auth == Data.ByteArray.convert authCalc
        then return  (Data.ByteArray.convert out)
        else CryptoFailed Crypto.Error.CryptoError_AuthenticationTagSizeInvalid
