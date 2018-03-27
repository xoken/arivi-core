module Network.Arivi.Types 
(
PayLoad     (..),
Frame       (..),
Socket,
SockAddr 
) where 

import           Network.Socket     
import qualified Data.ByteString.Char8              as C 
import qualified Data.Map.Strict                    as Map 
import           Data.UUID                                  (UUID)
import           Data.Int                                   (Int16,Int64)   


type SessionId      = Int64  
type PayLoadLength  = Int16    
type FragmentNumber = Integer 
type MessageId      = UUID 
type SubProtocol    = Int 

data Frame   = Frame {
                    version        :: Version 
                ,   publicFlags    :: PublicFlags
                ,   encoding       :: Encoding  
                ,   transport      :: Transport
                ,   opcode         :: Opcode 
                ,   fragmentNumber :: FragmentNumber
                ,   payLoadLength  :: PayLoadLength     
                ,   payLoadMarker  :: PayLoadMarker 
                ,   payLoad        :: PayLoad 
               } deriving (Show)

data Version
    = V0 
    | V1 
    deriving (Eq, Ord, Show)

data PublicFlags  = PublicFlags {
                    finalFragment :: Bool 
                ,   initiator     :: Bool 
                ,   ecncryption   :: EncryptionType 
            } deriving (Show) 

data EncryptionType = NONE 
                      | AES256_CTR
                      | CHACHA_POLY 
                      deriving (Show)

data Encoding = UTF_8
                | ASCII 
                | CBOR 
                | JSON 
                | PROTO_BUFF 
                deriving (Show)

data Transport = UDP 
                 | TCP 
                 deriving (Show)

data Opcode       = ERROR 
                    | HANDSHAKE_REQUEST 
                    | HANDSHAKE_REPONSE 
                    | OPTIONS 
                    | RESET 
                    | CLOSE 
                    | PING  
                    | PONG 
                    deriving (Show)
               

data PayLoadMarker = PayLoadMarker {
                    subProtocol    :: SubProtocol 
                ,   messageId      :: MessageId
            } deriving (Show)
                      
data PayLoad = PayLoad C.ByteString
               deriving (Show)
                