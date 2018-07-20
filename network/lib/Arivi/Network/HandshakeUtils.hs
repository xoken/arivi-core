module Arivi.Network.HandshakeUtils
    ( createHandshakeInitMsg
    , generateInitParcel
    , readHandshakeMsg
    , verifySignature
    , extractSecrets
    , createHandshakeRespMsg
    , generateRespParcel
    , readHandshakeResp
    , generateEphemeralKeys
    , EphemeralKeyPair
    ) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305
import qualified Arivi.Crypto.Utils.PublicKey.Encryption as Encryption
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Network.Connection                as Conn (CompleteConnection,
                                                                  IncompleteConnection,
                                                                  connectionId,
                                                                  mkCompleteConnection,
                                                                  remoteNodeId,
                                                                  sharedSecret)
import           Arivi.Network.Exception
import           Arivi.Network.Types                     (ConnectionId, HandshakeInitMasked (..),
                                                          HandshakeRespMasked (..),
                                                          Header (..),
                                                          Parcel (..),
                                                          Payload (..),
                                                          SerialisedMsg,
                                                          Version (..))
import           Arivi.Network.Utils
import           Codec.Serialise
import           Control.Exception                       (mapException, throw)
import           Crypto.ECC                              (SharedSecret)
import qualified Crypto.PubKey.Curve25519                as Curve25519
import qualified Crypto.PubKey.Ed25519                   as Ed25519
import qualified Data.ByteString.Lazy                    as L
import           Data.Int                                (Int64)

type EphemeralKeyPair = (Ed25519.SecretKey, Curve25519.PublicKey)

type EphemeralPublicKey = Curve25519.PublicKey

type EphemeralPrivateKey = Ed25519.SecretKey

getAeadNonceInitiator :: Int64
getAeadNonceInitiator = 1 :: Int64

getAeadNonceRecipient :: Int64
getAeadNonceRecipient = 2 ^ (64 :: Int64)-- Need to get precise value

getVersion :: [Version]
getVersion = [V0]

generateInitiatorNonce :: Int64
generateInitiatorNonce = 1

generateRecipientNonce :: Int64
generateRecipientNonce = 1

generateEphemeralKeys :: IO EphemeralKeyPair
generateEphemeralKeys = do
    (eSKSign, _) <- generateSigningKeyPair
    let mEphemeralPublicKey = getEncryptionPubKeyFromSigningSecretKey eSKSign
    return (eSKSign, mEphemeralPublicKey)

-- | Update the ephemeralPubKey, ephemeralSecretKey and shared secret in the connection structure
updateCryptoParams ::
       Conn.IncompleteConnection -> SharedSecret -> Conn.CompleteConnection
updateCryptoParams = mkCompleteConnection

-- | Takes the static secret key and connectionId and returns an encoded handshakeInitMsg as a lazy bytestring along with the updated connection object
createHandshakeInitMsg ::
       Ed25519.SecretKey
    -> Conn.IncompleteConnection
    -> EphemeralPrivateKey
    -> (SerialisedMsg, Conn.CompleteConnection)
createHandshakeInitMsg sk conn eSKSign = mapException NetworkCryptoException (serialise hsInitMsg, updatedConn)
  where
    mRemoteNodeId = Conn.remoteNodeId conn
    myNodeId = generateNodeId sk
    ssk =
        createSharedSecretKey
            (getEncryptionPublicKeyFromNodeId mRemoteNodeId)
            eSKSign
    staticssk =
        createSharedSecretKey
            (getEncryptionPublicKeyFromNodeId mRemoteNodeId)
            sk
    sign = signMsg sk (Encryption.sharedSecretToByteString staticssk)
    initNonce = generateInitiatorNonce
    hsInitMsg =
        HandshakeInitMessage
            getVersion
            (Conn.connectionId conn)
            initNonce
            myNodeId
            sign
    -- Consider using lenses for updation
    updatedConn = updateCryptoParams conn ssk

-- Takes the connection object and creates the response msg
createHandshakeRespMsg :: ConnectionId -> SerialisedMsg
createHandshakeRespMsg mConnectionId = serialise hsRespMsg
  where
    hsRespMsg = HandshakeRespMsg getVersion generateRecipientNonce mConnectionId

-- | Encrypt the hs init msg and return a parcel
generateInitParcel ::
       SerialisedMsg -> EphemeralPublicKey -> Conn.CompleteConnection -> Parcel
generateInitParcel msg mEphemeralPublicKey conn = mapException NetworkCryptoException $
    Parcel headerData (Payload $ strictToLazy ctWithMac)
  where
    aeadnonceInitiator = getAeadNonceInitiator
    headerData = HandshakeInitHeader mEphemeralPublicKey aeadnonceInitiator
    ssk = Conn.sharedSecret conn
    ctWithMac =
        encryptMsg
            aeadnonceInitiator
            ssk
            (L.toStrict $ serialise headerData)
            (L.toStrict msg)

-- | Encrypt the given message and return a response parcel
generateRespParcel ::
       SerialisedMsg -> SharedSecret -> EphemeralPublicKey -> Parcel
generateRespParcel msg ssk mEphemeralPublicKey = mapException NetworkCryptoException $
    Parcel headerData (Payload $ strictToLazy ctWithMac)
  where
    aeadnonceRecipient = getAeadNonceRecipient
    headerData = HandshakeRespHeader mEphemeralPublicKey aeadnonceRecipient
    ctWithMac =
        encryptMsg
            aeadnonceRecipient
            ssk
            (L.toStrict $ serialise headerData)
            (L.toStrict msg)

-- Update the connection object with the final shared secret key
extractSecrets ::
       Conn.IncompleteConnection
    -> EphemeralPublicKey
    -> EphemeralPrivateKey
    -> Conn.CompleteConnection
extractSecrets conn remoteEphPubKey myEphemeralSK = mapException NetworkCryptoException updatedConn
  where
    sskFinal = createSharedSecretKey remoteEphPubKey myEphemeralSK
    updatedConn = mkCompleteConnection conn sskFinal

-- | Read msg and return the header, ct, ephNodeId and aeadNonce
readParcel :: Parcel -> (Header, L.ByteString, EphemeralPublicKey, Int64)
readParcel hsParcel = (hsHeader, ciphertextWithMac, senderEphPubKey, aeadnonce)
    -- Need to check for right opcode. If not throw exception which should be caught appropriately. Currently, assume that we get KEY_EXCHANGE_INIT.
  where
    hsHeader = header hsParcel
    ciphertextWithMac = getPayload $ encryptedPayload hsParcel
    senderEphPubKey = ephemeralPublicKey hsHeader
    aeadnonce = aeadNonce hsHeader

-- | Receiver handshake
readHandshakeMsg ::
       Ed25519.SecretKey -> Parcel -> (HandshakeInitMasked, EphemeralPublicKey)
readHandshakeMsg sk parcel = mapException NetworkCryptoException (hsInitMsg, senderEphPubKey)
  where
    (hsHeader, ciphertextWithMac, senderEphPubKey, aeadnonce) =
        readParcel parcel
    ssk = createSharedSecretKey senderEphPubKey sk
    (ct, tag) = getCipherTextAuthPair (L.toStrict ciphertextWithMac)
    hsInitMsgSerialised =
        decryptMsg aeadnonce ssk (L.toStrict $ serialise hsHeader) tag ct
    hsInitMsgOrFail = deserialiseOrFail $ strictToLazy hsInitMsgSerialised
    hsInitMsg =
        case hsInitMsgOrFail of
            Left e    -> throw $ NetworkDeserialiseException e
            Right msg -> msg

-- | Reads the handshake response from the receiver and returns the message along with the updated connection object which stores the final ssk
readHandshakeResp ::
       Conn.IncompleteConnection
    -> EphemeralPrivateKey
    -> Parcel
    -> (HandshakeRespMasked, Conn.CompleteConnection)
readHandshakeResp conn ephemeralPrivateKey parcel = mapException NetworkCryptoException (hsRespMsg, updatedConn)
  where
    (hsHeader, ciphertextWithMac, receiverEphPubKey, aeadnonce) =
        readParcel parcel
    -- The final shared secret is derived and put into the connection object
    -- NOTE: Need to delete the ephemeral key pair from the connection object as it is not needed once shared secret key is derived
    updatedConn = extractSecrets conn receiverEphPubKey ephemeralPrivateKey
    (ct, tag) = getCipherTextAuthPair (L.toStrict ciphertextWithMac)
    hsRespMsgSerialised =
        decryptMsg
            aeadnonce
            (Conn.sharedSecret updatedConn)
            (L.toStrict $ serialise hsHeader)
            tag
            ct
    hsRespMsgOrFail = deserialiseOrFail $ strictToLazy hsRespMsgSerialised
    hsRespMsg =
        case hsRespMsgOrFail of
            Left e    -> throw $ NetworkDeserialiseException e
            Right msg -> msg

verifySignature :: Ed25519.SecretKey -> HandshakeInitMasked -> Bool
verifySignature sk msg = mapException NetworkCryptoException $
    verifyMsg
        remoteStaticNodeId
        (Encryption.sharedSecretToByteString staticssk)
        sign
  where
    sign = signature msg
    remoteStaticNodeId = nodePublicKey msg
    staticssk =
        createSharedSecretKey
            (getEncryptionPublicKeyFromNodeId remoteStaticNodeId)
            sk
