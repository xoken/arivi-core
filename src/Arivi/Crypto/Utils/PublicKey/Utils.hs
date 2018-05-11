module Arivi.Crypto.Utils.PublicKey.Utils
(
    getSignaturePublicKey,
    getSignaturePublicKeyFromNodeId,
    signMsg,
    verifyMsg,
    getEncryptionSecretKey,
    getEncryptionPublicKey,
    getEncryptionPublicKeyFromNodeId,
    createSharedSecretKey,
    deriveSharedSecretKey,
    generateSigningKeyPair,
    generateNodeId,
    encryptMsg,
    decryptMsg
) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305
import qualified Arivi.Crypto.Utils.PublicKey.Encryption as Encryption
import qualified Arivi.Crypto.Utils.PublicKey.Signature  as Signature
import           Arivi.Crypto.Utils.Random
import           Crypto.ECC                              (SharedSecret)
import           Crypto.Error                            (CryptoFailable,
                                                          throwCryptoError)
import           Crypto.Hash                             (Digest, SHA256, hash)
import qualified Crypto.PubKey.Curve25519                as Curve25519
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import           Data.ByteArray                          (convert)
import           Data.ByteString.Char8                   (ByteString, concat,
                                                          length, splitAt,
                                                          unpack)

-- | Wrapper function for getting the signature public key, given private key
getSignaturePublicKey :: Ed25519.SecretKey -> Ed25519.PublicKey
getSignaturePublicKey = Signature.getPublicKey

-- | Function for getting signature pk from nodeId
getSignaturePublicKeyFromNodeId :: ByteString -> Ed25519.PublicKey
getSignaturePublicKeyFromNodeId nodeId = throwCryptoError pk where
    pkPair = Data.ByteString.Char8.splitAt 32 nodeId
    pkBs = fst pkPair
    pk = Ed25519.publicKey pkBs

-- | Wrapper function for signing a message given just the sk and msg
signMsg :: Ed25519.SecretKey -> ByteString -> Ed25519.Signature
signMsg sk = Ed25519.sign sk pk where
    pk = getSignaturePublicKey sk

verifyMsg :: ByteString -> ByteString -> Ed25519.Signature -> Bool
verifyMsg nodeId = Ed25519.verify pk where
    pk = getSignaturePublicKeyFromNodeId nodeId

-- | Wrapper function for getting EncryptionSecretKey from SignatureSecretKey
getEncryptionSecretKey :: Ed25519.SecretKey -> Curve25519.SecretKey
getEncryptionSecretKey = Encryption.getSecretKey

-- | Wrapper function for getting the encryption public key, given private key
getEncryptionPublicKey :: Curve25519.SecretKey -> Curve25519.PublicKey
getEncryptionPublicKey = Encryption.getPublicKey

-- | Function for getting encryption pk from nodeId
getEncryptionPublicKeyFromNodeId :: ByteString -> Curve25519.PublicKey
getEncryptionPublicKeyFromNodeId nodeId = throwCryptoError pk where
    pkPair = Data.ByteString.Char8.splitAt 32 nodeId
    pkBs = snd pkPair
    pk = Curve25519.publicKey pkBs

-- | Takes the secret key (signSK) and the nodeId of remote and calls the Encryption.createSharedSecretKey with appropriate arguements
createSharedSecretKey :: Ed25519.SecretKey -> ByteString -> SharedSecret
createSharedSecretKey signSK remoteNodeId = Encryption.createSharedSecretKey encryptSK encryptPK where
        encryptSK = getEncryptionSecretKey signSK
        encryptPK = getEncryptionPublicKeyFromNodeId remoteNodeId

-- | Takes the master secret key (signSK) and the nodeId of remote and calls the Encryption.deriveSharedSecretKey with appropriate arguements
deriveSharedSecretKey :: ByteString -> Ed25519.SecretKey -> SharedSecret
deriveSharedSecretKey remoteNodeId signSK = Encryption.derivedSharedSecretKey encryptPK encryptSK where
        encryptSK = getEncryptionSecretKey signSK
        encryptPK = getEncryptionPublicKeyFromNodeId remoteNodeId


generateSigningKeyPair :: IO (Ed25519.SecretKey, Ed25519.PublicKey)
generateSigningKeyPair = Signature.generateKeyPair


generateNodeId :: Ed25519.SecretKey -> ByteString
generateNodeId signsk = Data.ByteString.Char8.concat [signingPK, encryptionPK] where
    signingPK = Signature.toByteString (getSignaturePublicKey signsk)
    encryptionPK = Encryption.toByteString (getEncryptionPublicKey (getEncryptionSecretKey signsk))

-- | Simple wrapper over chacha encryption
encryptMsg :: ByteString -> SharedSecret -> ByteString -> ByteString -> ByteString
encryptMsg aeadnonce ssk header msg = throwCryptoError $ chachaEncrypt aeadnonce sskBS header msg where
        sskBS = Encryption.sharedSecretToByteString ssk

-- | Simple wrapper over chacha decryption
decryptMsg :: ByteString -> SharedSecret -> ByteString -> ByteString -> ByteString -> ByteString
decryptMsg aeadnonce ssk header tag ct = throwCryptoError $ chachaDecrypt aeadnonce sskBS header tag ct where
        sskBS = Encryption.sharedSecretToByteString ssk

