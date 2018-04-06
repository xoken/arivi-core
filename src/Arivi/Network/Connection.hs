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
    ConnectionInfo (..),
    getUniqueConnectionId,
    genConnectionId,
    createConnection,
    closeConnection
) where


import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)

import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Kademlia.Types               (HostAddress, NodeId)
import           Arivi.Network.Types                (PortNumber, TransportType)





-- | ConnectionId is type synonym for ByteString
type ConnectionId = ByteString
type State = ByteString

-- | (ConnectionId,ConnectionInfo) are (key,value) pair in HashMap that stores
-- information about all the Connection uniquely
data ConnectionInfo = ConnectionInfo {
                          connectionId    :: ConnectionId
                        , nodeId          :: Keys.PublicKey
                        , ipAddress       :: HostAddress
                        , port            :: PortNumber
                        , ePhemeralPubKey :: Keys.PublicKey
                        , transportType   :: TransportType
                        , state           :: State
                        , sharedSecret    :: Keys.SharedSecret
                        } deriving (Eq)



-- | Generates a random 4 Byte ConnectionId using Raaz's random ByteString
-- generation
genConnectionId :: IO ByteString
genConnectionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Generates unique ConnectionInfo by checking it is already present in given
-- HashMap
getUniqueConnectionId :: HashMap ByteString ConnectionInfo -> IO ByteString
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
                 -> Keys.SharedSecret
                 -> HashMap ConnectionId ConnectionInfo
                 -> IO (ConnectionId,HashMap ConnectionId ConnectionInfo)
createConnection nodeId ipAddress port ePhemeralPubKey
                transportType state sharedSecret connectionHashmap =

                getUniqueConnectionId connectionHashmap
                    >>= \uniqueConnectionId
                    -> return
                    (uniqueConnectionId,
                        Data.HashMap.Strict.insert uniqueConnectionId
                                 (ConnectionInfo uniqueConnectionId nodeId
                                                ipAddress port ePhemeralPubKey
                                             transportType state sharedSecret)
                              connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
-- HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId ConnectionInfo
             -> HashMap ConnectionId ConnectionInfo
closeConnection = Data.HashMap.Strict.delete
