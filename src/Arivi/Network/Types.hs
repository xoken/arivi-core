{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Arivi.Network.Types
(
    Header         (..),
    Payload        (..),
    Parcel         (..),
    PeerType (..),
    SessionId      (..),
    MessageId,
    EncryptionType (..),
    TransportType  (..),
    EncodingType   (..),
    ContextID      (..),
    Version        (..),
    Socket,
    SockAddr,
    PortNumber,
    HostAddress,
    SerialisedMsg,
    PlainText,
    CipherText,
    NodeId,
    OutboundFragment,
    ServiceId (..),
    ConnectionId,
    Opcode(..),
    SequenceNum,
    FragmentNumber,
    -- FragmentCount,
    HandshakeInitMasked(..),
    HandshakeRespMasked(..),
    makeDataParcel,
    deserialise,
    serialise
) where

import           Codec.Serialise
import           Codec.Serialise.Class
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Crypto.Error
import           Crypto.PubKey.Curve25519 (PublicKey, publicKey)
import           Crypto.PubKey.Ed25519    (Signature, signature)
import           Data.ByteArray
import qualified Data.ByteString
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Int                 (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict          as Map
import           Data.Monoid
import           Data.UUID                (UUID)
import qualified Data.Word
import           GHC.Generics
import           Network.Socket

type ConnectionId   = ByteString
type SessionId      = Int32
-- need to be changed to Int24
type PayloadLength  = Int16
type FragmentNumber = Int16
-- type FragmentCount  = Int16
type MessageId      = ByteString
type ServiceId      = Int8
type Descriptor     = Data.ByteString.Char8.ByteString
type ContextID      = Int
type ServiceContext = Int32
type SerialisedMsg = BSL.ByteString

type SequenceNum = Integer -- ^ TODO Control.Concurrent.STM.Counter
-- | Following are the ranges for the aead nonces
type InitiatorNonce = Integer -- 1++
type RecipientNonce = Integer -- 2^32++
-- | This is the nonce for preventing replays. Don't need ranges for sender and receiver
type Nonce = Integer
type NodeId = ByteString
type PlainText = ByteString
type CipherText = ByteString
-- The different messages we can get from the network
data Opcode = KEY_EXCHANGE_INIT
            | KEY_EXCHANGE_RESP
            | DATA
            deriving (Show,Eq, Generic)

-- | This message is encrypted and sent in the handshake message
data HandshakeInitMasked = HandshakeInitMessage {
      versionList   :: [Version]
    , connectionId  :: ConnectionId
    , nonce         :: InitiatorNonce
    , nodePublicKey :: NodeId
    , signature     :: Signature
} deriving (Show, Eq, Generic)

data HandshakeRespMasked = HandshakeRespMsg {
    versionList    :: [Version]
    , nonce        :: RecipientNonce
    , connectionId :: ConnectionId
} deriving (Show, Eq, Generic)


-- | These are the different types of Headers for different types of Parcels
data Header = HandshakeHeader {
                    opcode             :: Opcode      -- ^ Denotes `Opcode`
                ,   ephemeralPublicKey :: NodeId      -- ^ `PublicKey` for
                                                      --    current Session
                ,   aeadNonce          :: ByteString  -- ^ 12 Byte Nonce used
                                                      --   for encryption
            }
            | DataHeader {
                    opcode          :: Opcode         -- ^ Denotes `Opcode`
                ,   messageId       :: MessageId      -- ^ Unique Message
                                                      --   Identifier
                ,   fragmentNumber  :: FragmentNumber -- ^ Number of fragment
                ,   totalFragements :: FragmentNumber -- ^ Total fragments in
                                                      --   current Message
                ,   connectionId    :: ConnectionId   -- ^ Connection Identifier
                                                      --   for particular
                                                      --   connection
                ,   nonce           :: Nonce          -- ^ Nonce which
                                                      --   increments by one
                                                      --   after each message
                                                      --   is sent. Useful for
                                                      --   preventing Replay
                                                      --   Attacks
            }
            | ErrorHeader {
                    opcode          :: Opcode         -- ^ Denotes `Opcode`
                ,   messageId       :: MessageId      -- ^ Unique Message
                                                      --   Identifier
                ,   fragmentNumber  :: FragmentNumber -- ^ Number of fragment
                ,   totalFragements :: FragmentNumber -- ^ Total fragments in
                                                      --   current Message
                ,   descriptor      :: Descriptor     -- ^ Shows type of error
                                                      --   and other fields
                ,   connectionId    :: ConnectionId   -- ^ Connection Identifier
                                                      --   for particular
                                                      --   connection
            }
            | ByeHeader {
                    opcode          :: Opcode         -- ^ Denotes `Opcode`
                ,   fragmentNumber  :: FragmentNumber -- ^ Number of fragment
                ,   totalFragements :: FragmentNumber -- ^ Total fragments in
                                                      --   current Message
                ,   connectionId    :: ConnectionId   -- ^ Connection Identifier
                                                      --   for particular
                                                      --   connection
                ,   messageId       :: MessageId      -- ^ Unique Message
                                                      --   Identifier
            }

             deriving (Show,Eq,Generic)

-- | This is pseudo frame which contains `Header` and `CipherText`.These fields
--   will be encoded using CBOR format. `CipherText` is encrypted `P2PMessage`
--   which is received from the Arivi P2P Layer. `Header` will not be encrypted
--   so it will be useful at the receiving side to know which type of Parcel is
--   this. After encoding these fields length of the parcel in Plain Text is
--   appended before sending on the Network.

data Parcel = Parcel {
                header           :: Header      -- ^ Header of the Parcel
              , encryptedPayload :: Payload     -- ^ Encrypted `P2PMessage`
            }
            deriving (Show,Eq,Generic)



-- | This is the structure that goes out on the wire
-- Has been tested for cborg encoding, decoding successfully
-- data Parcel   =  KeyExParcel {
--                     opcode              :: Opcode
--                 ,   handshakeCiphertext :: ByteString
--                 ,   ephemeralPublicKey  :: NodeId
--                 ,   aeadNonce           :: ByteString
--                }

--                | DataParcel  {
--                     opcode          :: Opcode
--                 ,   messageId       :: MessageId
--                 ,   fragmentNumber  :: FragmentNumber
--                 ,   totalFragements :: FragmentCount
--                 ,   connectionId    :: ConnectionId
--                 ,   payloadLength   :: PayloadLength
--                 ,   payload         :: Payload
--                 ,   nonce           :: Nonce
--                }

--                | ErrorParcel {
--                     opcode          :: Opcode
--                 ,   messageId       :: MessageId
--                 ,   fragmentNumber  :: FragmentNumber
--                 ,   totalFragements :: FragmentCount
--                 ,   descriptor      :: Descriptor
--                 ,   connectionId    :: ConnectionId
--                }

--                | ByeParcel {
--                     opcode          :: Opcode
--                 ,   fragmentNumber  :: FragmentNumber
--                 ,   totalFragements :: FragmentCount
--                 ,   connectionId    :: ConnectionId
--                 ,   messageId       :: MessageId
--                }
--                 deriving (Show,Eq,Generic)

-- makeDataParcel :: Opcode
--                -> MessageId
--                -> FragmentNumber
--                -> FragmentNumber
--                -> ConnectionId
--                -> PayloadLength
--                -> Payload
--                -> Nonce
--                -> Parcel
makeDataParcel = Parcel

type OutboundFragment = (MessageId, FragmentNumber, FragmentNumber, Payload)

data PeerType = INITIATOR | RECIPIENT
                deriving (Eq)

data Version
    = V0
    | V1
    deriving (Eq, Ord, Show,Generic)


data EncryptionType = NONE
                      | AES256_CTR
                      | CHACHA_POLY
                      deriving (Eq,Show,Generic)

data EncodingType =
                UTF_8
                | ASCII
                | CBOR
                | JSON
                | PROTO_BUFF
                deriving (Eq,Show,Generic)

data TransportType =
                   UDP
                 | TCP
                 deriving (Eq,Show,Generic)

newtype Payload = Payload {getPayload :: BSL.ByteString}
               deriving (Show,Eq,Generic)


instance Serialise Version
instance Serialise Opcode
instance Serialise EncodingType
instance Serialise TransportType
instance Serialise EncryptionType
instance Serialise Payload
instance Serialise Header
instance Serialise Parcel
instance Serialise HandshakeInitMasked
instance Serialise HandshakeRespMasked

-- Serialise intance for PublicKey
instance Serialise PublicKey where
    encode = encodePublicKey
    decode = decodePublicKey

encodePublicKey :: PublicKey -> Encoding
encodePublicKey bytes = do
    let temp = convert bytes :: Data.ByteString.ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodePublicKey :: Decoder s PublicKey
decodePublicKey = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of
        (2,0)  -> throwCryptoError . publicKey <$>
                    (decode :: Decoder s Data.ByteString.ByteString)
        _      -> fail "invalid PublicKey encoding"


-- Serilaise instance for Signature
instance Serialise Signature where
    encode = encodeSignature
    decode = decodeSignature

encodeSignature :: Signature -> Encoding
encodeSignature bytes = do
    let temp = convert bytes :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodeSignature :: Decoder s Signature
decodeSignature = do
    len <- decodeListLen
    tag <- decodeWord
    case (len,tag) of
        (2,0) ->  throwCryptoError . Crypto.PubKey.Ed25519.signature <$>
                    (decode :: Decoder s ByteString)
        _      -> fail "invalid Signature encoding"
