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
import           Arivi.Network.Connection                as Conn (Connection (..))
import           Arivi.Network.Types                     (HandshakeInitMasked (..),
                                                          HandshakeRespMasked (..),
                                                          Header (..), NodeId,
                                                          Opcode (..),
                                                          Parcel (..),
                                                          Payload (..),
                                                          SerialisedMsg,
                                                          Version (..))
import           Arivi.Network.Utils
import           Arivi.Utils.Exception                   (AriviException (AriviDeserialiseException))
import           Codec.Serialise
import           Control.Exception                       (throw)
import           Crypto.ECC                              (SharedSecret)
import           Crypto.Error
import qualified Crypto.PubKey.Curve25519                as Curve25519
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import qualified Data.Binary                             as Binary (decode,
                                                                    encode)
import           Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy                    as L
import           Data.Int                                (Int64)


getAeadNonceInitiator :: Int64
getAeadNonceInitiator = 1 :: Int64


getAeadNonceRecipient :: Int64
getAeadNonceRecipient = 20001::Int64 -- Need to get precise value

getVersion :: [Version]
getVersion = [V0]

generateInitiatorNonce :: Integer
generateInitiatorNonce = 1

generateRecipientNonce :: Integer
generateRecipientNonce = 1


generateEphemeralKeys :: Conn.Connection -> IO Conn.Connection
generateEphemeralKeys conn = do
    (eSKSign, _) <- generateSigningKeyPair
    let ephermeralPublicKey = getEncryptionPubKeyFromSigningSecretKey eSKSign
    let newconn = conn {Conn.ephemeralPubKey = ephermeralPublicKey, Conn.ephemeralPrivKey = eSKSign}
    return newconn

-- | Update the ephemeralPubKey, ephemeralSecretKey and shared secret in the connection structure
updateCryptoParams :: Conn.Connection -> Curve25519.PublicKey -> Ed25519.SecretKey -> SharedSecret -> Conn.Connection
updateCryptoParams conn epk esk ssk = conn {Conn.ephemeralPubKey = epk, Conn.ephemeralPrivKey = esk, Conn.sharedSecret = ssk}

-- | Takes the static secret key and connectionId and returns an encoded handshakeInitMsg as a lazy bytestring along with the updated connection object
createHandshakeInitMsg :: Ed25519.SecretKey -> Conn.Connection -> (SerialisedMsg, Conn.Connection)
createHandshakeInitMsg sk conn = (serialise hsInitMsg, updatedConn) where
    eSKSign = Conn.ephemeralPrivKey conn
    remoteNodeId = Conn.remoteNodeId conn
    myNodeId = generateNodeId sk
    ephermeralNodeId = Conn.ephemeralPubKey conn

    ssk = createSharedSecretKey (getEncryptionPublicKeyFromNodeId remoteNodeId) eSKSign
    staticssk = createSharedSecretKey (getEncryptionPublicKeyFromNodeId remoteNodeId) sk

    sign = signMsg sk (Encryption.sharedSecretToByteString staticssk)
    initNonce = generateInitiatorNonce
    hsInitMsg = HandshakeInitMessage getVersion (Conn.connectionId conn)initNonce myNodeId sign
    -- Consider using lenses for updation
    updatedConn = updateCryptoParams conn ephermeralNodeId eSKSign ssk

-- Takes the connection object and creates the response msg
createHandshakeRespMsg :: Conn.Connection -> SerialisedMsg
createHandshakeRespMsg conn = serialise hsRespMsg where
    hsRespMsg = HandshakeRespMsg getVersion generateRecipientNonce (Conn.connectionId conn)

-- | Encrypt the hs init msg and return a parcel
generateInitParcel :: SerialisedMsg -> Conn.Connection -> Parcel
generateInitParcel msg conn = Parcel headerData (Payload $ strictToLazy ctWithMac) where
    aeadnonceInitiator = getAeadNonceInitiator
    headerData = HandshakeInitHeader (Conn.ephemeralPubKey conn) aeadnonceInitiator
    ssk = Conn.sharedSecret conn
    ctWithMac = encryptMsg aeadnonceInitiator ssk (L.toStrict $ serialise headerData) (L.toStrict msg)

-- | Encrypt the given message and return a response parcel
generateRespParcel :: SerialisedMsg -> Conn.Connection -> Parcel
generateRespParcel msg conn = Parcel headerData (Payload $ strictToLazy ctWithMac) where
    aeadnonceRecipient = getAeadNonceRecipient
    headerData = HandshakeRespHeader (Conn.ephemeralPubKey conn) aeadnonceRecipient
    ssk = Conn.sharedSecret conn
    ctWithMac = encryptMsg aeadnonceRecipient ssk (L.toStrict $ serialise headerData) (L.toStrict msg)


-- Update the connection object with the final shared secret key
extractSecrets :: Conn.Connection -> Curve25519.PublicKey -> Ed25519.SecretKey -> Conn.Connection
extractSecrets conn remoteEphPubKey myEphemeralSK = updatedConn where
    sskFinal = createSharedSecretKey remoteEphPubKey myEphemeralSK
    updatedConn = conn {Conn.sharedSecret = sskFinal}

-- | Read msg and return the header, ct, ephNodeId and aeadNonce
readParcel :: Parcel -> (Header, L.ByteString, Curve25519.PublicKey,  Int64)
readParcel hsParcel = (hsHeader, ciphertextWithMac, senderEphPubKey, aeadnonce) where
    -- Need to check for right opcode. If not throw exception which should be caught appropriately. Currently, assume that we get KEY_EXCHANGE_INIT.
    hsHeader = header hsParcel
    ciphertextWithMac = getPayload $ encryptedPayload hsParcel
    senderEphPubKey = ephemeralPublicKey hsHeader
    aeadnonce = aeadNonce hsHeader

-- | Receiver handshake
readHandshakeMsg :: Ed25519.SecretKey -> Conn.Connection -> Parcel -> (HandshakeInitMasked, Curve25519.PublicKey)
readHandshakeMsg sk conn parcel = (hsInitMsg, senderEphPubKey) where
    (hsHeader, ciphertextWithMac, senderEphPubKey, aeadnonce) = readParcel parcel
    ssk = createSharedSecretKey senderEphPubKey sk
    (ct, tag) = getCipherTextAuthPair (L.toStrict ciphertextWithMac)
    hsInitMsgSerialised = decryptMsg aeadnonce ssk (L.toStrict $ serialise hsHeader) tag ct
    hsInitMsgOrFail = deserialiseOrFail $ strictToLazy hsInitMsgSerialised
    hsInitMsg = case hsInitMsgOrFail of
                    Left e    -> throw $ AriviDeserialiseException e
                    Right msg -> msg


-- | Reads the handshake response from the receiver and returns the message along with the updated connection object which stores the final ssk
readHandshakeResp :: Conn.Connection -> Parcel -> (HandshakeRespMasked, Conn.Connection)
readHandshakeResp conn parcel = (hsRespMsg, updatedConn) where
    (hsHeader, ciphertextWithMac, receiverEphPubKey, aeadnonce) = readParcel parcel
    -- The final shared secret is derived and put into the connection object
    -- NOTE: Need to delete the ephemeral key pair from the connection object as it is not needed once shared secret key is derived
    updatedConn = extractSecrets conn receiverEphPubKey (Conn.ephemeralPrivKey conn)
    (ct, tag) = getCipherTextAuthPair (L.toStrict ciphertextWithMac)
    hsRespMsgSerialised = decryptMsg aeadnonce (Conn.sharedSecret updatedConn) (L.toStrict $ serialise hsHeader) tag ct
    hsRespMsgOrFail = deserialiseOrFail $ strictToLazy hsRespMsgSerialised
    hsRespMsg = case hsRespMsgOrFail of
                    Left e    -> throw $ AriviDeserialiseException e
                    Right msg -> msg


verifySignature :: Ed25519.SecretKey -> HandshakeInitMasked -> Bool
verifySignature sk msg = verifyMsg remoteStaticNodeId (Encryption.sharedSecretToByteString staticssk) sign where
    sign = signature msg
    remoteStaticNodeId = nodePublicKey msg
    staticssk = createSharedSecretKey (getEncryptionPublicKeyFromNodeId remoteStaticNodeId) sk
