-- |
-- Module      :  Arivi.Network.Connection
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication
module Arivi.Network.Connection
(
    ConnectionId,
    PeerType (..),
    Connection (..),
    getUniqueConnectionId,
    genConnectionId,
    createConnection,
    closeConnection
) where


import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import           Network.Socket                     (Socket)

import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Kademlia.Types               (HostAddress, NodeId)
import           Arivi.Network.Types                (PortNumber, TransportType, Frame(..), ServiceRequest)
import           Control.Concurrent.STM.TChan       (TChan)




-- | ConnectionId is type synonym for ByteString
type ConnectionId = ByteString
type State = ByteString
data PeerType = INITIATOR | RECIPIENT
                deriving (Eq)

-- | (ConnectionId,Connection) are (key,value) pair in HashMap that stores
-- information about all the Connection uniquely
data Connection = Connection {
                          connectionId    :: ConnectionId
                        , nodeId          :: Keys.PublicKey
                        , ipAddress       :: HostAddress
                        , port            :: PortNumber
                        , ePhemeralPubKey :: Keys.PublicKey
                        , transportType   :: TransportType
                        , state           :: State
                        , peerType        :: PeerType
                        , sharedSecret    :: Keys.SharedSecret
                        , socket          :: Socket
                        , serviceRequestTChannel :: TChan ServiceRequest
                        , frameTChannel      :: TChan Frame
                        } deriving (Eq)
-- connectionsid = id || port || transportType
-- data ServiceType =  OPEN
--                   | CLOSED
--                   | SENDMSG
--                   deriving (Show,Eq)
-- type Message = String
-- type IP = String
-- type Port = String
-- type NodeId = String

-- data ServiceRequest = ServiceRequest {
--                          serviceType :: ServiceType
--                         ,message     :: Message
--                         ,ipAddress   :: HostAddress
--                         , port       :: PortNumber
--                         , nodeId     :: Keys.PublicKey
--                     } deriving (Show,Eq)



-- data ConnectionLayer2 = ConnectionLayer2 {
--                          connectionId           :: ConnectionId
--                         , transportType         :: TransportType
--                         , nodeId                :: Keys.PublicKey
--                         , ipAddress             :: HostAddress
--                         , serviceRequesttTChan  :: TChan ServiceRequest
--                         , frameTChan            :: TChan Frame
--                         }


-- | Generates a random 4 Byte ConnectionId using Raaz's random ByteString
-- generation
genConnectionId :: IO ByteString
genConnectionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Generates unique Connection by checking it is already present in given
-- HashMap
getUniqueConnectionId :: HashMap ByteString Connection -> IO ByteString
getUniqueConnectionId hashmap = do
                                connectionId <- genConnectionId

                                if member connectionId hashmap
                                    then  getUniqueConnectionId hashmap
                                    else
                                        return connectionId




-- | Creates Unique Connection  and stores in given HashMap

createConnection :: Keys.PublicKey
                 -> HostAddress
                 -> PortNumber
                 -> Keys.PublicKey
                 -> TransportType
                 -> State
                 -> PeerType
                 -> Keys.SharedSecret
                 -> Socket
                 -> HashMap ConnectionId Connection
                 -> TChan ServiceRequest
                 -> TChan Frame
                 -> IO (ConnectionId,HashMap ConnectionId Connection)
createConnection nodeId ipAddress port ePhemeralPubKey
                transportType state peerType sharedSecret socket connectionHashmap serviceRequestTChan frameTChan =

                getUniqueConnectionId connectionHashmap
                    >>= \uniqueConnectionId
                    -> return
                    (uniqueConnectionId,
                        Data.HashMap.Strict.insert uniqueConnectionId
                                 (Connection uniqueConnectionId nodeId
                                                ipAddress port ePhemeralPubKey
                                             transportType state peerType sharedSecret
                                             socket serviceRequestTChan frameTChan)
                              connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
-- HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId Connection
             -> HashMap ConnectionId Connection
closeConnection = Data.HashMap.Strict.delete
