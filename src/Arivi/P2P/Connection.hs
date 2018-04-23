-- |
-- Module      :  Arivi.P2P.Connection
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication
module Arivi.P2P.Connection
(
    ConnectionId,
    Connection (..),
    ServiceRequest(..),
    ServiceType(..),
    Message,
    createConnection,
    closeConnection,
    genConnectionId,
    getUniqueConnectionId
) where


import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, pack,append)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import           Network.Socket                     (Socket)
import           Control.Concurrent.STM.TChan

import qualified Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Kademlia.Types               (HostAddress, NodeId)
import           Arivi.Network.Types                (PortNumber, TransportType,Frame)





-- | ConnectionId is type synonym for ByteString
type ConnectionId = ByteString

-- type State = ByteString


data ServiceType =  OPEN
                  | CLOSED
                  | SENDMSG
                  deriving (Show,Eq)

-- | Message is ByteString
type Message = ByteString
-- type ServiceType = ByteString


data ServiceRequest = ServiceRequest {
                          -- requestType :: RequestType
                          serviceType :: ServiceType
                        , message     :: Message
                        -- , connection   :: Connection
                        -- , nodeId1                :: Keys.PublicKey
                        -- , ipAddress1             :: HostAddress
                        -- , port1                  :: PortNumber
                        -- , transportType1         :: TransportType

                    } deriving (Show,Eq)



-- | (ConnectionId,Connection) are (key,value) pair in HashMap that stores
--   information about all the Connection uniquely

data Connection = Connection {
                      connectionId          :: ConnectionId
                    , nodeId                :: Keys.PublicKey
                    , ipAddress             :: HostAddress
                    , port                  :: PortNumber
                    , transportType         :: TransportType
                    , serviceRequestTChan   :: TChan ServiceRequest
                    , frameTChan            :: TChan Frame -- TODO change frame to payload
                    -- TODO initiator or not
                    } deriving (Eq)




-- | Generates a random 4 Byte ConnectionId using Raaz's random ByteString
--   generation
genConnectionId :: IO ByteString
genConnectionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Generates unique Connection by checking it is already present in given
--   HashMap
getUniqueConnectionId :: HashMap ByteString Connection -> IO ByteString
getUniqueConnectionId hashmap = do
                                connectionId <- genConnectionId

                                if member connectionId hashmap
                                    then  getUniqueConnectionId hashmap
                                    else
                                        return connectionId

makeConnectionId ipAddress port transportType =
                        return (concanate
                                  (concanate (concanate ipAddress "|")
                                             (concanate port "|"))
                                  (concanate transportType "|"))


concanate first second = Data.ByteString.Char8.append
                            (Data.ByteString.Char8.pack $ show first)
                            (Data.ByteString.Char8.pack $ show second)



-- | Creates Unique Connection  and stores in given HashMap

-- createConnection :: Keys.PublicKey
--                  -> HostAddress
--                  -> PortNumber
--                  -> TransportType
--                  -> TChan ServiceRequest
--                  -> TChan Frame
--                  -> HashMap ConnectionId Connection
--                  -> IO (ConnectionId,HashMap ConnectionId Connection)
createConnection nodeId ipAddress port transportType serviceRequestTChan
                    frameTChan connectionHashmap =

                makeConnectionId ipAddress port transportType
                    >>= \uniqueConnectionId
                    -> return
                    (uniqueConnectionId,
                        Data.HashMap.Strict.insert uniqueConnectionId
                                 (Connection uniqueConnectionId
                                             nodeId
                                             ipAddress
                                             port
                                             transportType
                                             serviceRequestTChan
                                             frameTChan)
                                 connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
--   HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId Connection
             -> HashMap ConnectionId Connection
closeConnection = Data.HashMap.Strict.delete
