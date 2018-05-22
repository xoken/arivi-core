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
    -- ParcelCipher,
    getUniqueConnectionId,
    genConnectionId,
    createConnection,
    closeConnection,
    concatenate,
    makeConnectionId
) where


import           Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Types                (ConnectionId, Event (..),
                                                     NodeId, OutboundFragment,
                                                     Parcel (..), PeerType (..),
                                                     PortNumber, SequenceNum,
                                                     TransportType)
import           Arivi.P2P.Types                    (ServiceRequest (..))
import           Control.Concurrent.STM.TChan       (TChan)
import qualified Crypto.PubKey.Curve25519           as Curve25519
import qualified Crypto.PubKey.Ed25519              as Ed25519
import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, append, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)
import qualified Network.Socket                     as Network (HostAddress,
                                                                Socket)



-- type State = ByteString

-- type ServiceRequest = ByteString
-- | (ConnectionId,Connection) are (key,value) pair in HashMap that stores
-- information about all the Connection uniquely
-- type ParcelCipher = ByteString

data Connection = Connection {
                          connectionId          :: ConnectionId
                        , remoteNodeId          :: NodeId
                        , ipAddress             :: Network.HostAddress
                        , port                  :: PortNumber
                        , ephemeralPubKey       :: Curve25519.PublicKey
                        , ephemeralPrivKey      :: Ed25519.SecretKey
                        , transportType         :: TransportType
                        , peerType              :: PeerType
                        , socket                :: Network.Socket
                        , sharedSecret          :: Keys.SharedSecret
                        , eventTChan            :: TChan Event
                        , outboundFragmentTChan :: TChan OutboundFragment
                        , egressSeqNum          :: SequenceNum
                        , ingressSeqNum         :: SequenceNum
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



-- | Takes two arguments converts them into ByteString and concatenates them
concatenate :: (Show first, Show second) => first -> second -> ByteString
concatenate first second = Data.ByteString.Char8.append
                            (Data.ByteString.Char8.pack $ show first)
                            (Data.ByteString.Char8.pack $ show second)


-- | ConnectionId is concatenation of IP Address, PortNumber and TransportType
makeConnectionId :: (Monad m)
                 => Network.HostAddress
                 -> PortNumber
                 -> TransportType
                 -> m ConnectionId
makeConnectionId ipAddress port transportType =
                        return (concatenate
                                  (concatenate (concatenate ipAddress "|")
                                               (concatenate port "|"))
                                  (concatenate transportType "|"))

-- | Creates Unique Connection  and stores in given HashMap

createConnection :: NodeId
                 -> Network.HostAddress
                 -> PortNumber
                 -> Curve25519.PublicKey
                 -> Ed25519.SecretKey
                 -> TransportType
                 -> PeerType
                 -> Network.Socket
                 -> Keys.SharedSecret
                 -> TChan Event
                 -> TChan OutboundFragment
                 -> SequenceNum
                 -> SequenceNum
                 -> HashMap ConnectionId Connection
                 -> IO (ConnectionId,HashMap ConnectionId Connection)
createConnection nodeId ipAddress port ephemeralPubKey ephemeralPrivKey
                transportType  peerType socket sharedSecret eventTChan outboundFragmentTChan egressSeqNum ingressSeqNum
                                                         connectionHashmap =

                getUniqueConnectionId connectionHashmap
                    >>= \uniqueConnectionId
                    -> return
                    (uniqueConnectionId,
                        Data.HashMap.Strict.insert uniqueConnectionId
                                 (Connection uniqueConnectionId nodeId
                                                ipAddress port ephemeralPubKey
                                                ephemeralPrivKey
                                             transportType peerType socket
                                             sharedSecret eventTChan outboundFragmentTChan
                                             egressSeqNum ingressSeqNum)
                              connectionHashmap)




-- | Closes connection for given connectionId, which deletes the member of
-- HashMap identified by connectionId

closeConnection :: ConnectionId
             -> HashMap ConnectionId Connection
             -> HashMap ConnectionId Connection
closeConnection = Data.HashMap.Strict.delete
