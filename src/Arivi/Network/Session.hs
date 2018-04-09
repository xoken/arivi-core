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
    ServiceCode,
    Session (..),
    createSession,
    closeSession
) where


import           Data.ByteString.Char8    (ByteString, append, pack)
import           Data.HashMap.Strict      (HashMap, delete, empty, insert,
                                           member)

import           Arivi.Network.Connection
import           Arivi.Network.Types      (EncodingType, ServiceContext,
                                           TransportType)

-- | SessionId is type synonym for ByteString
type SessionId = ByteString

-- | ServiceCode is type synonym for ByteString
type ServiceCode = ByteString




-- | (sessionId,Session) are (key,value) pair in HashMap that stores
-- information about all the session uniquely
data Session = Session {
                          sessionId      :: SessionId
                        , serviceContext :: ServiceContext
                        , connectionId   :: ConnectionId
                        , encodingType   :: EncodingType
                        } deriving (Eq)



-- | Creates Unique session and stores in given HashMap

createSession :: ServiceCode
              -> ServiceContext
              -> ConnectionId
              -> EncodingType
              -> HashMap SessionId Session
              -> (SessionId,HashMap SessionId Session)
createSession serviceCode serviceContext connectionId
              encodingType sessionHashmap =
        (Data.ByteString.Char8.append serviceCode connectionId,
            Data.HashMap.Strict.insert
                (Data.ByteString.Char8.append serviceCode connectionId)
                (Session
                    (Data.ByteString.Char8.append serviceCode connectionId)
                        serviceContext
                        connectionId encodingType)
                        sessionHashmap)


-- | Closes Session for given sessionId, which deletes the member of HashMap
--  identified by SessionId

closeSession :: SessionId
             -> HashMap SessionId Session
             -> HashMap SessionId Session
closeSession = Data.HashMap.Strict.delete
