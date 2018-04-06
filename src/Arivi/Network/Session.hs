-- |
-- Module      :  Arivi.Network.Session
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing sessions in Arivi
-- communication

module Arivi.Network.Session
(
    SessionId,
    Session (..),
    getUniqueSessionId,
    genSessionId,
    createSession,
    closeSession
) where


import           Data.ByteString.Base16             (encode)
import           Data.ByteString.Char8              (ByteString, pack)
import           Data.HashMap.Strict                (HashMap, delete, empty,
                                                     insert, member)

import           Arivi.Crypto.Utils.Keys.Encryption as Encryption
import           Arivi.Crypto.Utils.Random          (getRandomByteString)
import           Arivi.Kademlia.Types               (HostAddress, NodeId)
import           Arivi.Network.Connection
import           Arivi.Network.Types                (EncodingType,
                                                     ServiceContext,
                                                     TransportType)

-- | SessionId is type synonym for ByteString
type SessionId = ByteString

-- | (sessionId,Session) are (key,value) pair in HashMap that stores
-- information about all the session uniquely
data Session = Session {
                          sessionId      :: SessionId
                        , serviceContext :: ServiceContext
                        , connectionId   :: ConnectionId
                        , encodingType   :: EncodingType
                        } deriving (Eq)






-- | Generates a random 4 Byte SessionId using Raaz's random ByteString
-- generation
genSessionId :: IO SessionId
genSessionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Generates unique SessionId by checking it is already present in given
-- HashMap
getUniqueSessionId :: HashMap ByteString Session -> IO SessionId
getUniqueSessionId hashmap = do
                                sessionId <- genSessionId

                                if member sessionId hashmap
                                    then  getUniqueSessionId hashmap
                                    else
                                        return sessionId




-- | Creates Unique session and stores in given HashMap

createSession :: ServiceContext
             -> ConnectionId
             -> EncodingType
             -> HashMap SessionId Session
             -> IO (SessionId,HashMap SessionId Session)
createSession serviceContext connectionId
              encodingType sessionHashmap =

                getUniqueSessionId sessionHashmap
                    >>= \uniqueSessionId
                    -> return
                   (uniqueSessionId,
                    Data.HashMap.Strict.insert uniqueSessionId
                                 (Session uniqueSessionId serviceContext
                                    connectionId encodingType)
                              sessionHashmap)


-- | Closes Session for given sessionId, which deletes the member of HashMap
--  identified by SessionId

closeSession :: SessionId
             -> HashMap SessionId Session
             -> HashMap SessionId Session
closeSession = Data.HashMap.Strict.delete
