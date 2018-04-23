-- |
-- Module      :  Arivi.P2P.Session
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing sessions in Arivi
-- communication

module  Arivi.P2P.Session
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

import qualified Arivi.Crypto.Utils.Keys.Encryption as Keys
import           Arivi.Network.Connection
import           Arivi.Network.Types      (EncodingType, ServiceCode,
                                           TransportType,HostAddress,
                                           PortNumber)
import           Arivi.P2P.ServiceRegistry (ServiceContext)

-- | SessionId is type synonym for ByteString
type SessionId = ByteString
type NodeId = Keys.PublicKey

  -- data SessionRequest = SessionRequest {
  --     -- type of OPEN / CLOSE
  --     -- UUID v4 for session identifier (both)
  --     -- transport type (open)
  --     -- port (open)
  --     -- IP (open)
  --     -- reason
  -- }

  -- data ServiceContext = ServiceContext {
  --     TChan SessionRequest
  -- }

-- | (sessionId,Session) are (key,value) pair in HashMap that stores
-- information about all the session uniquely
data Session = Session {
                          sessionId      :: SessionId
                        , serviceContext :: ServiceContext
                        , connectionId   :: ConnectionId
                        , encodingType   :: EncodingType
                        } deriving (Eq)


-- https://github.com/xoken/arivi/blob/WireSpecification/AriviWireSpec.md
-- | Creates Unique session and stores in given HashMap
createSession :: ServiceCode
              -> ServiceContext
              -> ConnectionId
              -> EncodingType
              -> HashMap SessionId Session
              -> (SessionId,HashMap SessionId Session)

-- createSession :: ServiceCode
--               -> ServiceContext
--               -> ConnectionId
--               -> EncodingType
--               -> HashMap SessionId Session
--               -> (SessionId,HashMap SessionId Session)
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
