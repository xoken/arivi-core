module Arivi.Network.Handshake
(
    doInitHandshake,
    receiveHandshake
) where


import           Arivi.Crypto.Cipher.ChaChaPoly1305
import qualified Arivi.Crypto.Utils.PublicKey.Encryption as Encryption
import qualified Arivi.Crypto.Utils.PublicKey.Signature  as Signature
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Types                     (HandshakeInitMasked (..),
                                                          HandshakeRespMasked (..),
                                                          Opcode (..),
                                                          Parcel (..))
import           Codec.Serialise
import           Crypto.Error
import qualified Crypto.PubKey.Curve25519                as Curve25519
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import           Data.ByteArray
import           Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy                    as L

generateNonce :: IO ByteString
generateNonce = getRandomByteString 12


-- | Takes the parcel to be sent, the remote's nodeId and encrypts the parcel using remote's public key and a newly generated ephemeral private key and returns the encrypted message with the appended mac tag along with nonce and ephemeral public key
doInitHandshake versions connId nodeId staticSK remoteNodeId = do
        -- | Generate signing (master) ephemeral keypair
        (eSKSign, ePKSign) <- generateSigningKeyPair
        let ssk = createSharedSecretKey eSKSign remoteNodeId
        let staticssk = createSharedSecretKey staticSK remoteNodeId
        let signature = signMsg eSKSign (Encryption.sharedSecretToByteString staticssk)
        -- Generate the nonce
        let nonce = 1
        let hsmsg = HandshakeMessage versions connId nonce nodeId signature
        print hsmsg
        let encodedHsMsg = serialise hsmsg
        aeadnonce <- generateNonce
        -- -- Don't know what this parameter is used for. Passed in chachaencrypt function
        let header = B.empty
        -- -- Encrypt and mac the parcel
        let ciphertextWithMac = throwCryptoError $ chachaEncrypt aeadnonce ssk header (L.toStrict encodedHsMsg)
        let hsParcel = KeyExInitParcel KEY_EXCHANGE_INIT ciphertextWithMac (generateNodeId eSKSign) aeadnonce

        return (serialise hsParcel)

-- | Minimal testing
receiveHandshake nodeId staticSK dataParcel = do
    let parcel = deserialise dataParcel :: Parcel
    let ciphertextWithMac = handshakeInitCiphertext parcel
    let senderNodeId = ephemeralPublicKey parcel
    let encryptionNonce = aeadNonce parcel
    let ssk = createSharedSecretKey staticSK senderNodeId
    let (ct, tag) = getCipherTextAuthPair ciphertextWithMac
    let hsMsgDecrypted = throwCryptoError $ chachaDecrypt encryptionNonce ssk B.empty tag ct
    let hsMsg = deserialise (L.fromStrict hsMsgDecrypted) :: HandshakeInitMasked
    let sign = signature hsMsg
    let remoteStaticNodeId = nodePublicKey hsMsg
    let staticssk = createSharedSecretKey staticSK remoteStaticNodeId
    print (verifyMsg senderNodeId (Encryption.sharedSecretToByteString staticssk) sign)
    print hsMsg

