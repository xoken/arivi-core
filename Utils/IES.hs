-- | 
-- Module     : Crypto.Utils.IES
-- Maintainer :
--
-- IES with Elliptic curve  <https://en.wikipedia.org/wiki/Integrated_Encryption_Scheme>
--
-- This is simple cryptographic scheme based on Crypto.PubKey.ECIES
-- Library <https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-ECIES.html>
-- this is used can be used for symmetric key encrpytion where sender will
-- generate shared secret key  and ephemeral public key using public key of 
-- receiver and sends ephemeral public key to receiver using some key exchange 
-- mechanism (such as Diffie-Hellman) and using receiver generates same shared
-- secret key using received ephemeral public key


module Crypto.Utils.IES 
(
    generateSharedSecret,
    genIESParams
) where


import Crypto.PubKey.ECIES
import Crypto.ECC
import Crypto.Error
import Crypto.PubKey.Curve25519
import Data.Proxy
import Data.ByteString.Char8


-- |Proxy Curve represenation this is standard curve known to both sender and 
-- receiver
curve = Proxy :: Proxy Curve_X25519



-- | genIESParams takes Public Key of the receiver and generates IESParams 
--  containing ephemeral public key and shared secret key tuple
genIESParams receiversPK = do 
                             ies <- (deriveEncrypt curve receiversPK)
                             return (throwCryptoError ies)



-- | This is used at receiver's side , ephemeral Public Key and receiver's
-- secret key is used to generate shared secret key of communication 

generateSharedSecret ePubKey receiversSK = (throwCryptoError 
                                            (deriveDecrypt curve ePubKey 
                                                (throwCryptoError receiversSK)))

