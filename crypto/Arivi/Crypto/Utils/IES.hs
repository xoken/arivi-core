-- IES with Elliptic curve  <https://en.wikipedia.org/wiki/Integrated_Encryption_Scheme>
--
-- This is simple cryptographic scheme based on Crypto.PubKey.ECIES
-- Library <https://hackage.haskell.org/package/cryptonite-0.25/docs/Crypto-PubKey-ECIES.html>
-- this is used can be used for symmetric key encryption where sender will
-- generate shared secret key  and ephemeral public key using public key of
-- receiver and sends ephemeral public key to receiver using some key exchange
-- mechanism (such as Diffie-Hellman) and using receiver generates same shared
-- secret key using received ephemeral public key
--
module Arivi.Crypto.Utils.IES
    ( generateSharedSecret
    , genIESParams
    ) where

import Crypto.ECC
import Crypto.Error
import Crypto.PubKey.Curve25519
import Crypto.PubKey.ECIES
import Crypto.Random.Types
import Data.Proxy

-- |Proxy Curve representation this is standard curve known to both sender and
-- receiver
curve :: Proxy Curve_X25519
curve = Proxy :: Proxy Curve_X25519

-- | genIESParams takes Public Key of the receiver and generates IESParams
--  containing ephemeral public key and shared secret key tuple
genIESParams :: MonadRandom m => PublicKey -> m (CryptoFailable (PublicKey, SharedSecret))
genIESParams = deriveEncrypt curve

-- | This is used at receiver's side , ephemeral Public Key and receiver's
-- secret key is used to generate shared secret key of communication
generateSharedSecret :: PublicKey -> CryptoFailable SecretKey -> CryptoFailable SharedSecret
generateSharedSecret ePubKey receiversSK = deriveDecrypt curve ePubKey (throwCryptoError receiversSK)
