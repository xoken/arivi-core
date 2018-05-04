-- |
-- Module      :  Arivi.Network.Connection
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication
module Arivi.Network.Connection
(
    ConnectionId,
    Connection (..),
    getUniqueConnectionId,
    genConnectionId,
    createConnection,
    closeConnection
) where


import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import qualified Crypto.PubKey.Ed25519               as Ed25519
import           Arivi.Crypto.Utils.Random
import           Arivi.Kademlia.Types               (HostAddress)
import           Arivi.Network.Types                (Parcel (..), PeerType (..),
                                                     PortNumber, SequenceNum,
                                                     TransportType, NodeId, ConnectionId)
import           Arivi.P2P.Types                    (ServiceRequest (..))
import           Control.Concurrent.STM.TChan       (TChan)
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import           Network.Socket                     (Socket)



-- type State = ByteString

-- type ServiceRequest = ByteString
-- | (ConnectionId,Connection) are (key,value) pair in HashMap that stores
-- information about all the Connection uniquely
data Connection = Connection {
                          connectionId    :: ConnectionId
                        , remoteNodeId    :: NodeId
                        , ipAddress       :: HostAddress
                        , port            :: PortNumber
                        , ephemeralPubKey :: NodeId
                        , ephemeralPrivKey:: Ed25519.SecretKey
                        , transportType   :: TransportType
                        , peerType        :: PeerType
                        , sharedSecret    :: Keys.SharedSecret
                        , serviceReqTChan :: TChan ServiceRequest
                        , parcelTChan     :: TChan Parcel
                        , egressSeqNum    :: SequenceNum
                        , ingressSeqNum   :: SequenceNum
                        } deriving (Eq)



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

createConnection :: NodeId
                 -> HostAddress
                 -> PortNumber
                 -> NodeId
                 -> Ed25519.SecretKey
                 -> TransportType
                 -> PeerType
                 -> Keys.SharedSecret
                 -> HashMap ConnectionId Connection
                 -> TChan ServiceRequest
                 -> TChan Parcel
                 -> SequenceNum
                 -> SequenceNum
                 -> IO (ConnectionId,HashMap ConnectionId Connection)
createConnection nodeId ipAddress port ephemeralPubKey ephemeralPrivKey 
                transportType  peerType sharedSecret connectionHashmap
                serviceRequestTChan parcelTChan egressSeqNum ingressSeqNum =

                getUniqueConnectionId connectionHashmap
                    >>= \uniqueConnectionId
                    -> return
                    (uniqueConnectionId,
                        Data.HashMap.Strict.insert uniqueConnectionId
                                 (Connection uniqueConnectionId nodeId
                                                ipAddress port ephemeralPubKey ephemeralPrivKey 
                                             transportType peerType sharedSecret
                                             serviceRequestTChan
                                             parcelTChan
                                             egressSeqNum ingressSeqNum)
                              connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
-- HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId Connection
             -> HashMap ConnectionId Connection
closeConnection = Data.HashMap.Strict.delete
