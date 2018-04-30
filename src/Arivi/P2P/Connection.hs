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
    -- P2PRequest(..),
    -- Message,
    closeConnection,
    concatenate,
    createConnection,
    genConnectionId,
    getUniqueConnectionId,
    makeConnectionId
) where


import qualified Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Kademlia.Types               (HostAddress, NodeId)
import           Arivi.Network.Types                (PortNumber, TransportType)
import           Arivi.P2P.Types                    (ConnectionId,
                                                     P2PMessage (..),
                                                     ServiceRequest (..))
import           Control.Concurrent.STM.TChan
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, append, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import           Network.Socket                     (Socket)



-- | (ConnectionId,Connection) are (key,value) pair in HashMap that stores
--   information about all the Connection uniquely

data Connection = Connection {
                      connectionId        :: ConnectionId
                    , nodeId              :: Keys.PublicKey
                    , ipAddress           :: HostAddress
                    , port                :: PortNumber
                    , transportType       :: TransportType
                    , serviceRequestTChan :: TChan ServiceRequest
                    , messageTChan        :: TChan P2PMessage
                    , isInitiator         :: Bool
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

-- | ConnectionId is concatenation of IP Address, PortNumber and TransportType
makeConnectionId :: (Monad m)
                 => HostAddress
                 -> PortNumber
                 -> TransportType
                 -> m ConnectionId
makeConnectionId ipAddress port transportType =
                        return (concatenate
                                  (concatenate (concatenate ipAddress "|")
                                               (concatenate port "|"))
                                  (concatenate transportType "|"))


-- | Takes two arguments converts them into ByteString and concatenates them
concatenate :: (Show first, Show second) => first -> second -> ByteString
concatenate first second = Data.ByteString.Char8.append
                            (Data.ByteString.Char8.pack $ show first)
                            (Data.ByteString.Char8.pack $ show second)



-- | Creates Unique Connection  and stores in given HashMap

createConnection:: Monad m =>
        Keys.PublicKey
     -> HostAddress
     -> PortNumber
     -> TransportType
     -> TChan ServiceRequest
     -> TChan P2PMessage
     -> Bool
     -> HashMap ConnectionId Connection
     -> m (ConnectionId,HashMap ConnectionId Connection)

createConnection nodeId ipAddress port transportType serviceRequestTChan
                    frameTChan isInitiator connectionHashmap =

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
                                             frameTChan
                                             isInitiator)
                                 connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
--   HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId Connection
             -> HashMap ConnectionId Connection
closeConnection = Data.HashMap.Strict.delete
