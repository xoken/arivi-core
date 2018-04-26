{-# LANGUAGE DeriveGeneric #-}
module Arivi.Network.Types
(   Payload        (..),
    Parcel         (..),
    PeerType (..),
    SessionId      (..),
    MessageId      (..),
    EncryptionType (..),
    TransportType  (..),
    EncodingType   (..),
    ContextID      (..),
    Socket,
    SockAddr,
    PortNumber,
    HostAddress,
    Version,
    serialise,
    deserialise,
    -- ServiceContext (..),
    ServiceId (..),
    ConnectionId,
    -- ServiceCode,
    -- ServiceRequest(..),
    -- ServiceType(..),
    Opcode(..),
    SequenceNum
    -- Message
) where

import           Arivi.Crypto.Utils.Keys.Encryption as Encryption
import           Codec.Serialise
import           Codec.Serialise.Class
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Data.ByteArray
import qualified Data.ByteString
import           Data.ByteString.Char8              (ByteString)
import           Data.Int                           (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict                    as Map
import           Data.Monoid
import           Data.UUID                          (UUID)
import           GHC.Generics
import           Network.Socket

type ConnectionId   = Int32
type SessionId      = Int32
-- need to be changed to Int24
type PayloadLength  = Int16
type FragmentNumber = Int16
type MessageId      = String
type ServiceId      = Int8
type Descriptor     = Data.ByteString.Char8.ByteString
type ContextID      = Int
type ServiceContext = Int32

type SequenceNum = Integer
-- type Message = String

-- | ServiceCode is type synonym for ByteString
-- type ServiceCode = ByteString

-- The type of message we get from the p2p layer
-- data ServiceType =  INITIATE
--                   | TERMINATE
--                   | SENDMSG
--                   deriving (Show,Eq)

-- ServiceType plus optional message
-- data ServiceRequest = ServiceRequest {
--                         service     :: ServiceType
--                        ,message     :: Message
--                       } deriving (Show,Eq)


-- The different messages we can get from the network
data Opcode = VERSION_INIT
            | VERSION_RESP
            | KEY_EXCHANGE_INIT
            | KEY_EXCHANGE_RESP
            | DATA
            deriving (Show,Eq, Generic)


data Parcel   =  VersionParcel {
                    opcode             :: Opcode
                ,   versionList        :: [Version]
                ,   connectionId       :: ConnectionId
                }
               |KeyExDHInitParcel {
                    opcode             :: Opcode
                ,   versionList        :: [Version]
                ,   connectionId       :: ConnectionId
                ,   ePhemeralPublicKey :: Encryption.PublicKey
                ,   nodePublicKey      :: Encryption.PublicKey

               }
               |KeyExDHResponseParcel {
                    opcode             :: Opcode
                ,   versionList        :: [Version]
                ,   connectionId       :: ConnectionId
                ,   ePhemeralPublicKey :: Encryption.PublicKey
                ,   nodePublicKey      :: Encryption.PublicKey

               }
               | DataParcel  {
                    opcode         :: Opcode
                ,   messageId      :: MessageId
                ,   fragmentNumber :: FragmentNumber
                ,   connectionId   :: ConnectionId
                ,   payloadLength  :: PayloadLength
                ,   payload        :: Payload
               }

               | ErrorParcel {
                    opcode         :: Opcode
                ,   messageId      :: MessageId
                ,   fragmentNumber :: FragmentNumber
                ,   descriptor     :: Descriptor
                ,   connectionId   :: ConnectionId
               }

               | ByeParcel {
                    opcode         :: Opcode
                ,   fragmentNumber :: FragmentNumber
                ,   connectionId   :: ConnectionId
                ,   messageId      :: MessageId
               }
                deriving (Show,Eq,Generic)

data PeerType = INITIATOR | RECIPIENT
                deriving (Eq)

data Version
    = V0
    | V1
    deriving (Eq, Ord, Show,Generic)

-- data PublicFlags  = PublicFlags {
--                     finalFragment :: Bool
--                 ,   initiator     :: Bool
--                 ,   ecncryption   :: EncryptionType
--                 ,   encoding      :: EncodingType
--                 ,   transportType :: TransportType
--             } deriving (Show,Generic)

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

-- data Opcode       =   ERROR
--                     | HANDSHAKE_REQUEST
--                     | HANDSHAKE_REPONSE
--                     | OPTIONS
--                     | RESET
--                     | CLOSE
--                     | PING
--                     | PONG
--                     deriving (Show,Generic)


-- newtype PayloadMarker = PayloadMarker {
--                             serviceId :: ServiceId
--                     } deriving (Show,Generic)

newtype Payload = Payload Data.ByteString.Char8.ByteString
               deriving (Show,Eq,Generic)

instance Serialise Version
instance Serialise Opcode
instance Serialise EncodingType
instance Serialise TransportType
instance Serialise EncryptionType
-- instance Serialise PublicFlags
-- instance Serialise PayloadMarker
instance Serialise Payload
instance Serialise Parcel

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

