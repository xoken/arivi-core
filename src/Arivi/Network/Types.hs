{-# LANGUAGE DeriveGeneric #-}
module Arivi.Network.Types
(   PayLoad        (..),
    Frame          (..),
    SessionId      (..),
    MessageId      (..),
    EncryptionType (..),
    TransportType  (..),
    EncodingType   (..),
    SubProtocol    (..),
    ContextID      (..),
    Socket,
    SockAddr,
    PortNumber,
    HostAddress,
    Version,
    serialise,
    deserialise,
    ServiceContext (..)
) where

import           Arivi.Crypto.Utils.Keys.Encryption as Encryption
import           Codec.Serialise
import           Codec.Serialise.Class
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding
import           Data.ByteArray
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import           Data.Int                           (Int16, Int64)
import qualified Data.Map.Strict                    as Map
import           Data.Monoid
import           Data.UUID                          (UUID)
import           GHC.Generics
import           Network.Socket

type SessionId  = Int64
type PayLoadLength = Int16
type FragmentNumber = Integer
type MessageId  = String
type SubProtocol = Int
type Descriptor  = Data.ByteString.Char8.ByteString
type ContextID  = Int
type ServiceContext = Int


data Frame   =  HandshakeFrame {
                    versionList        :: [Version]
                ,   opcode             :: Opcode
                ,   sessionId          :: SessionId
                ,   messageId          :: MessageId
                ,   encodingModeList   :: [EncodingType]
                ,   encryptionModeList :: [EncryptionType]
                ,   ePhemeralPublicKey :: Encryption.PublicKey
                ,   remotePublicKey    :: Encryption.PublicKey

               }
               | RegularFrame  {
                    version        :: Version
                ,   opcode         :: Opcode
                ,   publicFlags    :: PublicFlags
                ,   messageId      :: MessageId
                ,   payLoadMarker  :: PayLoadMarker
                ,   fragmentNumber :: FragmentNumber
                ,   sessionId      :: SessionId
                ,   payLoadLength  :: PayLoadLength
                ,   payLoad        :: PayLoad
               }

               | ErrorFrame {
                    version        :: Version
                ,   opcode         :: Opcode
                ,   publicFlags    :: PublicFlags
                ,   messageId      :: MessageId
                ,   fragmentNumber :: FragmentNumber
                ,   descriptor     :: Descriptor
                ,   sessionId      :: SessionId
               }

               | ResetCloseFrame {
                    version        :: Version
                ,   opcode         :: Opcode
                ,   publicFlags    :: PublicFlags
                ,   fragmentNumber :: FragmentNumber
                ,   sessionId      :: SessionId
                ,   messageId      :: MessageId
               }
                deriving (Show,Generic)

data Version
    = V0
    | V1
    deriving (Eq, Ord, Show,Generic)

data PublicFlags  = PublicFlags {
                    finalFragment :: Bool
                ,   initiator     :: Bool
                ,   ecncryption   :: EncryptionType
                ,   encoding      :: EncodingType
                ,   transportType     :: TransportType
            } deriving (Show,Generic)

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

data Opcode       =   ERROR
                    | HANDSHAKE_REQUEST
                    | HANDSHAKE_REPONSE
                    | OPTIONS
                    | RESET
                    | CLOSE
                    | PING
                    | PONG
                    deriving (Show,Generic)


newtype PayLoadMarker = PayLoadMarker {
                            subProtocol :: SubProtocol
                    } deriving (Show,Generic)

newtype PayLoad = PayLoad Data.ByteString.Char8.ByteString
               deriving (Show,Generic)

instance Serialise Version
instance Serialise Opcode
instance Serialise EncodingType
instance Serialise TransportType
instance Serialise EncryptionType
instance Serialise PublicFlags
instance Serialise PayLoadMarker
instance Serialise PayLoad
instance Serialise Frame

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


