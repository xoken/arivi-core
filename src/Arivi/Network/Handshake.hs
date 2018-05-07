module Arivi.Network.Handshake
(
    initiatorHandshake,
    -- receiveHandshake
) where


import           Arivi.Crypto.Cipher.ChaChaPoly1305
import qualified Arivi.Crypto.Utils.PublicKey.Encryption as Encryption
import qualified Arivi.Crypto.Utils.PublicKey.Signature  as Signature
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Crypto.Utils.Random
import qualified Arivi.Network.Connection                as Conn
import           Arivi.Network.Types                     (ConnectionId, HandshakeInitMasked (..),
                                                          HandshakeRespMasked (..),
                                                          NodeId, Opcode (..),
                                                          Parcel (..),
                                                          Version (..))
import           Codec.Serialise
import           Crypto.ECC                              (SharedSecret)
import           Crypto.Error
import qualified Crypto.PubKey.Curve25519                as Curve25519
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import           Data.ByteArray
import           Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy                    as L


generateAeadNonce :: IO ByteString
generateAeadNonce = getRandomByteString 12

getVersion :: [Version]
getVersion = [V0]

generateInitiatorNonce :: Integer
generateInitiatorNonce = 1

generateRecipientNonce :: Integer
generateRecipientNonce = 100

getHeader :: B.ByteString
getHeader = B.empty

-- | Update the ephemeralPubKey, ephemeralSecretKey and shared secret in the connection structure
updateCryptoParams :: Conn.Connection -> NodeId -> Ed25519.SecretKey -> SharedSecret -> Conn.Connection
updateCryptoParams conn epk esk ssk = conn {Conn.ephemeralPubKey = epk, Conn.ephemeralPrivKey = esk, Conn.sharedSecret = ssk}


-- | Takes the static secret key and connectionId and returns an encoded handshakeInitMsg as a lazy bytestring
createHandshakeInitMsg :: Ed25519.SecretKey -> Conn.Connection -> IO (L.ByteString, Conn.Connection)
createHandshakeInitMsg sk conn = do
    (eSKSign, ePKSign) <- generateSigningKeyPair
    let remoteNodeId = Conn.remoteNodeId conn
    let myNodeId = generateNodeId sk
    let ephermeralNodeId = generateNodeId eSKSign

    let ssk = createSharedSecretKey eSKSign remoteNodeId
    let staticssk = createSharedSecretKey sk remoteNodeId

    let signature = signMsg eSKSign (Encryption.sharedSecretToByteString staticssk)
    let initNonce = generateInitiatorNonce
    let hsInitMsg = HandshakeInitMessage getVersion (Conn.connectionId conn)initNonce myNodeId signature
    print hsInitMsg
    -- Consider using lenses for updation
    let updatedConn = updateCryptoParams conn ephermeralNodeId eSKSign ssk
    return (serialise hsInitMsg, updatedConn)

-- | Simple wrapper over chacha encryption
encryptMsg :: B.ByteString -> SharedSecret -> B.ByteString -> B.ByteString -> B.ByteString
encryptMsg aeadnonce ssk header msg = throwCryptoError $ chachaEncrypt aeadnonce sskBS header msg where
        sskBS = Encryption.sharedSecretToByteString ssk

-- | Encrypt the given message and return a parcel
generateInitParcel :: B.ByteString -> Conn.Connection -> IO Parcel
generateInitParcel msg conn = do
        aeadnonce <- generateAeadNonce
        let ssk = Conn.sharedSecret conn
        let ctWithMac = encryptMsg aeadnonce ssk getHeader msg
        let eNodeId = Conn.ephemeralPubKey conn
        return (KeyExParcel KEY_EXCHANGE_INIT ctWithMac eNodeId aeadnonce)

-- | Takes the static secret key and connection object and returns a serialized KeyExParcel
initiatorHandshake :: Ed25519.SecretKey -> Conn.Connection -> IO L.ByteString
initiatorHandshake sk conn = do
        -- | Generate the serialised handshake init message
        (hsInitMsg, updatedConn) <- createHandshakeInitMsg sk conn
        hsParcel <- generateInitParcel (L.toStrict hsInitMsg) updatedConn
        return (serialise hsParcel)

-- | Receiver handshake
readHandshakeMsg :: Ed25519.SecretKey -> Conn.Connection -> L.ByteString -> IO(HandshakeInitMasked, NodeId)
readHandshakeMsg sk conn msg = do
    let hsParcel = deserialise msg :: Parcel
    -- Need to check for right opcode. If not throw exception which should be caught appropriately. Currently, assume that we get KEY_EXCHANGE_INIT.
    let ciphertextWithMac = handshakeCiphertext hsParcel
    let senderEphNodeId = ephemeralPublicKey hsParcel
    let aeadnonce = aeadNonce hsParcel
    let ssk = createSharedSecretKey sk senderEphNodeId
    let (ct, tag) = getCipherTextAuthPair ciphertextWithMac
    let hsInitMsgSerialised = throwCryptoError $ chachaDecrypt aeadnonce ssk B.empty tag ct
    let hsInitMsg = deserialise (L.fromStrict hsInitMsgSerialised) :: HandshakeInitMasked
    return (hsInitMsg, senderEphNodeId)


verifySignature :: Ed25519.SecretKey -> NodeId ->HandshakeInitMasked -> Bool
verifySignature sk senderEphNodeId msg = verifyMsg senderEphNodeId (Encryption.sharedSecretToByteString staticssk) sign where
    sign = signature msg
    remoteStaticNodeId = nodePublicKey msg
    staticssk = createSharedSecretKey sk remoteStaticNodeId

extractSecrets :: Conn.Connection -> NodeId -> Ed25519.SecretKey -> Conn.Connection
extractSecrets conn remoteEphNodeId myEphemeralSK = updatedConn where
    sskFinal = createSharedSecretKey myEphemeralSK remoteEphNodeId
    updatedConn = conn {Conn.sharedSecret = sskFinal}


createHandshakeRespMsg :: Conn.Connection -> L.ByteString
createHandshakeRespMsg conn = serialise hsRespMsg where
    hsRespMsg = HandshakeRespMsg getVersion generateRecipientNonce (Conn.connectionId conn)

-- | Encrypt the given message and return a parcel
generateRespParcel :: B.ByteString -> Conn.Connection -> IO Parcel
generateRespParcel msg conn = do
        aeadnonce <- generateAeadNonce
        let ssk = Conn.sharedSecret conn
        let ctWithMac = encryptMsg aeadnonce ssk getHeader msg
        let eNodeId = Conn.ephemeralPubKey conn
        return (KeyExParcel KEY_EXCHANGE_RESP ctWithMac eNodeId aeadnonce)

-- | Takes receiver static secret key, connection object and the received msg and returns an IO Lazy Bytestring
receiverHandshake :: Ed25519.SecretKey -> Conn.Connection -> L.ByteString -> IO L.ByteString
receiverHandshake sk conn msg = do
    (hsInitMsg, senderEphNodeId) <- readHandshakeMsg sk conn msg -- assuming msg to be a lazy bytestring
    print $ verifySignature sk senderEphNodeId hsInitMsg --if verification returns false, do something
    -- Generate an ephemeral keypair
    (eSKSign, ePKSign) <- generateSigningKeyPair
    let ephermeralNodeId = generateNodeId eSKSign
    let conn = conn {Conn.ephemeralPubKey = ephermeralNodeId, Conn.ephemeralPrivKey = eSKSign}
    -- Get updated connection structure
    let updatedConn = extractSecrets conn senderEphNodeId eSKSign
    let hsRespMsg = createHandshakeRespMsg conn
    hsRespParcel <- generateRespParcel (L.toStrict hsRespMsg) updatedConn
    return $ serialise hsRespMsg
