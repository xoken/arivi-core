module Network.Arivi.Types 
(
Payload     (..),
Frame       (..),
Socket,
SockAddr 
) where 

import           Network.Socket     
import qualified Data.ByteString.Char8              as C 
import qualified Data.Map.Strict                    as Map 
import           Data.UUID                                  (UUID)
import           Data.Int                                   (Int16,Int64)   

-- | Structure to hold the Payload which arivi will send and receive (PENDING)

type ConnectionId   = Int64  
type PayloadLength  = Int16    
type FragmentNumber = Integer 
type ContextId      = UUID 
type SubProtocol    = Int 

data Frame   = Frame {
                    version       :: Version 
                ,   payLoadMarker :: PayloadMarker 
                ,   opcode        :: Opcode 
                ,   publicFlags   :: PublicFlags     
                ,   payload       :: Payload 
                ,   connectionId  :: ConnectionId   
                ,   payloadLength :: PayloadLength 
            } deriving (Show)

data Version
    = V0 
    | V1 
    deriving (Eq, Ord, Show)

data PayloadMarker = PayloadMarker {
                    subProtocol    :: SubProtocol 
                ,   fragmentNumber :: FragmentNumber
                ,   contextId      :: ContextId
            } deriving (Show)

data Opcode       = REQUEST 
                    | RESPONSE 
                    | ERROR 
                    | HANDSHAKE_REQUEST 
                    | HANDSHAKE_REPONSE 
                    | OPTIONS 
                    | RESET 
                    | CLOSE 
                    | PING  
                    | PONG 
                    deriving (Show)

data PublicFlags  = PublicFlags {
                    finalFragment :: Bool 
                ,   textOrBinary  :: Bool 
                ,   initiator     :: Bool 
                ,   ecncryption   :: EncryptionType 
            } deriving (Show)

data EncryptionType = NONE 
                      | AES_CTR
                      | CHA_CHA_POLY 
                      deriving (Show)
                      
data Payload = Payload C.ByteString 
               deriving (Show)
               

                    