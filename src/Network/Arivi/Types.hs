

module Network.Arivi.Types
(   PayLoad        (..),
    Frame          (..),
    SessionId      (..),
    MessageId      (..),
    EncryptionType (..),
    Transport      (..),
    Encoding       (..),
    SubProtocol    (..),
    ContextID      (..),
    Socket,
    SockAddr,
    PortNumber,
    HostAddress
) where

import qualified Data.ByteString.Char8
import           Data.Int              (Int16, Int64)
import qualified Data.Map.Strict       as Map
import           Data.UUID             (UUID)
import           Network.Socket
import           Arivi.Crypto.Utils.Keys.Encryption

type SessionId  = Int64
type PayLoadLength = Int16
type FragmentNumber = Integer
type MessageId  = UUID
type SubProtocol = Int
type EncodingList = Data.ByteString.Char8.ByteString
type EncryptionModeList  = Data.ByteString.Char8.ByteString
type Descriptor  = Data.ByteString.Char8.ByteString
type ContextID  = Int


data Frame   =  HandshakeFrame {
                    versionList        :: [Version]
                ,   opcode             :: Opcode
                ,   sessionId          :: SessionId
                ,   messageId          :: MessageId
                ,   encodingModeList   :: [EncodingList]
                ,   encryptionModeList :: [EncryptionModeList]
                ,   ePhemeralPublicKey :: Arivi.Crypto.Utils.Keys.Encryption.PublicKey
                ,   remotePublicKey    :: Arivi.Crypto.Utils.Keys.Encryption.PublicKey

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
                deriving (Show)

data Version
    = V0
    | V1
    deriving (Eq, Ord, Show)

data PublicFlags  = PublicFlags {
                    finalFragment :: Bool
                ,   initiator     :: Bool
                ,   ecncryption   :: EncryptionType
                ,   encoding      :: Encoding
                ,   transport     :: Transport
            } deriving (Show)

data EncryptionType = NONE
                      | AES256_CTR
                      | CHACHA_POLY
                      deriving (Eq,Show)

data Encoding = UTF_8
                | ASCII
                | CBOR
                | JSON
                | PROTO_BUFF
                deriving (Eq,Show)

data Transport =   UDP
                 | TCP
                 deriving (Eq,Show)

data Opcode       =   ERROR
                    | HANDSHAKE_REQUEST
                    | HANDSHAKE_REPONSE
                    | OPTIONS
                    | RESET
                    | CLOSE
                    | PING
                    | PONG
                    deriving (Show)


newtype PayLoadMarker = PayLoadMarker {
                            subProtocol :: SubProtocol
                    } deriving (Show)

newtype PayLoad = PayLoad Data.ByteString.Char8.ByteString
               deriving (Show)
