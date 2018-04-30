module Arivi.Crypto.Utils.PublicKey.NodeKeys
(
	generateNodeKeys, 
	getNodeId
) where

import           Arivi.Crypto.Utils.Random
import           Arivi.Crypto.Utils.PublicKey.Signature
import           Arivi.Crypto.Utils.PublicKey.Encryption
import           Data.ByteArray            (convert)
import           Data.ByteString.Char8     (ByteString, concat, unpack, length, splitAt)
import           Crypto.Error              (CryptoFailable, throwCryptoError)
import qualified Crypto.PubKey.Curve25519  (PublicKey, SecretKey, toPublic, secretKey)
import qualified Crypto.PubKey.Ed25519     (PublicKey, SecretKey)
import           Crypto.Hash               (SHA256, Digest, hash)

type EncryptionPubKey = Crypto.PubKey.Curve25519.PublicKey
type EncryptionPrivKey = Crypto.PubKey.Curve25519.SecretKey
type SigningPubKey = Crypto.PubKey.Ed25519.PublicKey
type SigningPrivKey = Crypto.PubKey.Ed25519.SecretKey

-- | Generates signing and encryption keypair for a node
generateNodeKeys :: IO (SigningPrivKey, SigningPubKey, EncryptionPrivKey, EncryptionPubKey)

generateNodeKeys = do
	(signSK, signPK) <- Arivi.Crypto.Utils.PublicKey.Signature.generateKeyPair
	(encryptSK, encryptPK) <- Arivi.Crypto.Utils.PublicKey.Encryption.generateKeyPair signSK
	return (signSK, signPK, encryptSK, encryptPK)

-- | NodeId representation = signPK || encryptPK
getNodeId :: IO ByteString
getNodeId = do
	(signSK, signPK, encryptSK, encryptPK) <- generateNodeKeys
	let nodeId = Data.ByteString.Char8.concat [Arivi.Crypto.Utils.PublicKey.Signature.toByteString signPK, Arivi.Crypto.Utils.PublicKey.Encryption.toByteString encryptPK]
	return (nodeId)

-- | Get (SignPK, EncryptPK)
getPublicKeys :: ByteString -> IO (ByteString, ByteString)
getPublicKeys nodeId = return (Data.ByteString.Char8.splitAt 32 nodeId)

-- main = do
-- 	nodeId <- getNodeId
-- 	print (Data.ByteString.Char8.length nodeId)
-- 	