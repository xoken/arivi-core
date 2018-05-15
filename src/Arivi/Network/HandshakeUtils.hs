module Arivi.Network.HandshakeUtils
(
    createHandshakeInitMsg,
    generateInitParcel,
    readHandshakeMsg,
    verifySignature,
    extractSecrets,
    createHandshakeRespMsg,
    generateRespParcel,
    readHandshakeResp,
    generateEphemeralKeys
) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305
import qualified Arivi.Crypto.Utils.PublicKey.Encryption as Encryption
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Crypto.Utils.Random
import qualified Arivi.Network.Connection                as Conn
import           Arivi.Network.Types                     (HandshakeInitMasked (..),
                                                          HandshakeRespMasked (..),
                                                          Header (..), NodeId,
                                                          Opcode (..),
                                                          Parcel (..),
                                                          Payload (..),
                                                          Version (..))
import           Arivi.Network.Utils
import           Codec.Serialise
import           Crypto.ECC                              (SharedSecret)
import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                   as Ed25519

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



generateEphemeralKeys :: Conn.Connection -> IO Conn.Connection
generateEphemeralKeys conn = do
    (eSKSign, _) <- generateSigningKeyPair
    let ephermeralNodeId = generateNodeId eSKSign
    let newconn = conn {Conn.ephemeralPubKey = ephermeralNodeId, Conn.ephemeralPrivKey = eSKSign}
    return newconn
-- | Update the ephemeralPubKey, ephemeralSecretKey and shared secret in the connection structure
updateCryptoParams :: Conn.Connection -> NodeId -> Ed25519.SecretKey -> SharedSecret -> Conn.Connection
updateCryptoParams conn epk esk ssk = conn {Conn.ephemeralPubKey = epk, Conn.ephemeralPrivKey = esk, Conn.sharedSecret = ssk}

-- | Takes the static secret key and connectionId and returns an encoded handshakeInitMsg as a lazy bytestring along with the updated connection object
createHandshakeInitMsg :: Ed25519.SecretKey -> Conn.Connection -> (L.ByteString, Conn.Connection)
createHandshakeInitMsg sk conn = (serialise hsInitMsg, updatedConn) where
    eSKSign = Conn.ephemeralPrivKey conn
    remoteNodeId = Conn.remoteNodeId conn
    myNodeId = generateNodeId sk
    ephermeralNodeId = Conn.ephemeralPubKey conn

    ssk = createSharedSecretKey eSKSign remoteNodeId
    staticssk = createSharedSecretKey sk remoteNodeId

    sign = signMsg eSKSign (Encryption.sharedSecretToByteString staticssk)
    initNonce = generateInitiatorNonce
    hsInitMsg = HandshakeInitMessage getVersion (Conn.connectionId conn)initNonce myNodeId sign
    -- Consider using lenses for updation
    updatedConn = updateCryptoParams conn ephermeralNodeId eSKSign ssk

-- Takes the connection object and creates the response msg
createHandshakeRespMsg :: Conn.Connection -> L.ByteString
createHandshakeRespMsg conn = serialise hsRespMsg where
    hsRespMsg = HandshakeRespMsg getVersion generateRecipientNonce (Conn.connectionId conn)


generateInitParcel :: B.ByteString -> Conn.Connection -> IO Parcel
generateInitParcel msg conn = do
    aeadnonce <- generateAeadNonce
    let header = HandshakeHeader KEY_EXCHANGE_INIT (Conn.ephemeralPubKey conn) aeadnonce
    let ssk = Conn.sharedSecret conn
    let ctWithMac = encryptMsg aeadnonce ssk (lazyToStrict (serialise header)) msg
    let parcel = Parcel header (Payload $ strictToLazy ctWithMac)
    return parcel

-- | Encrypt the given message and return a response parcel
generateRespParcel :: B.ByteString -> Conn.Connection -> IO Parcel
generateRespParcel msg conn = do
    aeadnonce <- generateAeadNonce
    let header = HandshakeHeader KEY_EXCHANGE_RESP (Conn.ephemeralPubKey conn) aeadnonce
    let ssk = Conn.sharedSecret conn
    let ctWithMac = strictToLazy $ encryptMsg aeadnonce ssk (lazyToStrict (serialise header)) msg
    let parcel = Parcel header (Payload ctWithMac)
    return parcel


-- Update the connection object with the final shared secret key
extractSecrets :: Conn.Connection -> NodeId -> Ed25519.SecretKey -> Conn.Connection
extractSecrets conn remoteEphNodeId myEphemeralSK = updatedConn where
    sskFinal = createSharedSecretKey myEphemeralSK remoteEphNodeId
    updatedConn = conn {Conn.sharedSecret = sskFinal}

-- | Receiver handshake
readHandshakeMsg :: Ed25519.SecretKey -> Conn.Connection -> L.ByteString -> (HandshakeInitMasked, NodeId)
readHandshakeMsg sk conn msg = (hsInitMsg, senderEphNodeId) where
    hsParcel = deserialise msg :: Parcel
    -- Need to check for right opcode. If not throw exception which should be caught appropriately. Currently, assume that we get KEY_EXCHANGE_INIT.
    hsHeader = header hsParcel
    ciphertextWithMac = getPayload $ encryptedPayload hsParcel
    senderEphNodeId = ephemeralPublicKey hsHeader
    aeadnonce = aeadNonce hsHeader
    ssk = createSharedSecretKey sk senderEphNodeId
    (ct, tag) = getCipherTextAuthPair (lazyToStrict ciphertextWithMac)
    hsInitMsgSerialised = throwCryptoError $ chachaDecrypt aeadnonce ssk (lazyToStrict (serialise hsHeader)) tag ct
    hsInitMsg = deserialise (strictToLazy hsInitMsgSerialised) :: HandshakeInitMasked

verifySignature :: Ed25519.SecretKey -> NodeId ->HandshakeInitMasked -> Bool
verifySignature sk senderEphNodeId msg = verifyMsg senderEphNodeId (Encryption.sharedSecretToByteString staticssk) sign where
    sign = signature msg
    remoteStaticNodeId = nodePublicKey msg
    staticssk = createSharedSecretKey sk remoteStaticNodeId

-- Reads the handshake response from the receiver and returns the message along with the updated connection object which stores the final ssk
readHandshakeResp :: Conn.Connection -> L.ByteString -> (HandshakeRespMasked, Conn.Connection)
readHandshakeResp conn msg = (hsInitMsg, updatedConn) where
    hsParcel = deserialise msg :: Parcel
    -- Need to check for right opcode. If not throw exception which should be caught appropriately. Currently, assume that we get KEY_EXCHANGE_RESP.
    hsHeader = header hsParcel
    ciphertextWithMac = getPayload $ encryptedPayload hsParcel
    receiverEphNodeId = ephemeralPublicKey hsHeader
    aeadnonce = aeadNonce hsHeader
    -- The final shared secret is derived and put into the connection object
    -- NOTE: Need to delete the ephemeral key pair from the connection object as it is not needed once shared secret key is derived
    updatedConn = extractSecrets conn receiverEphNodeId (Conn.ephemeralPrivKey conn)
    (ct, tag) = getCipherTextAuthPair (lazyToStrict ciphertextWithMac)
    hsInitMsgSerialised = throwCryptoError $ chachaDecrypt aeadnonce (Conn.sharedSecret updatedConn) B.empty tag ct
    hsInitMsg = deserialise (strictToLazy hsInitMsgSerialised) :: HandshakeRespMasked
