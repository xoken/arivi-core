{-# LANGUAGE MagicHash #-}
module KeyHandling
(
    getSecretKey,
    getPublicKey,
    sign,
    verify,
    toHex 
) where


import Crypto.PubKey.Ed25519 (SecretKey,PublicKey,secretKey,toPublic,sign,verify)
import Crypto.Error (throwCryptoError)
import Data.ByteString.Char8 
import Data.ByteArray (convert)
import Data.ByteString.Base16 (encode)

import Data.ByteArray 
import Xor 
import Random 
import Data.ByteString.Base16 as H
import Crypto.Util 
import GHC.Integer.Logarithms
import GHC.Exts

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


demo = do
    seed1 <- getRandomByteString 32
    seed2 <- getRandomByteString 32

    let sk1 = getSecretKey seed1
        sk2 = getSecretKey seed2 

        pk1 = getPublicKey sk1
        pk2 = getPublicKey sk2 

        h1 = H.encode (convert pk1 :: ByteString)
        h2 = H.encode (convert pk2 :: ByteString)

    let d1 = ((xor pk1 pk2) :: ByteString) 
    
    --print  ("Xor Distance : " ++ (Data.ByteString.Char8.unpack (h3)))

    let a = I# (integerLog2# (bs2i d1))
    print a 

test 0 = print "done"
test x = do 
            demo
            test(x-1) 