-- |
-- Module      :  Arivi.P2P.ServiceRegistry
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- ServiceRegistry is part of Arivi P2P layer, It keeps track of ServiceContext
-- and ConnectionCommand

module Arivi.P2P.ServiceRegistry
(
    ConnectionCommand(..),
    ContextId,
    ServiceContext(..)
) where

import           Arivi.Network.Types          (TransportType(..))
import           Arivi.P2P.Connection         (Connection, ConnectionId)
import           Arivi.P2P.Types              (ServiceCode)
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Data.HashMap.Strict          (HashMap)
import           Data.UUID                    (UUID)

-- | ContextId is type synonyms for UUID
type ContextId = UUID

-- | ConnectionCommand keeps track of Connection Shared Variable
data ConnectionCommand = ConnectionCommand {
           connectionId   :: ConnectionId   -- ^ Unique Connection Identifier
          ,connectionMVar :: MVar Connection-- ^ Shared Variable  for connection
          }

-- | Keeps information about Services
data ServiceContext = ServiceContext {
       contextId              :: ContextId               -- ^ Context Identifier
                                                         --   for services
      ,serviceCode            :: ServiceCode             -- ^ Service Code like
                                                         --   KDM,BSY,etc
      ,transportType          :: TransportType           -- ^ Transport Type
                                                         --   like `UDP` or
                                                         --   `TCP`
      ,outputTChan            :: TChan String            -- ^ This `TChan` is
                                                         --   used for receiving
                                                         --   message after
                                                         --   fragmentation
      ,connectionCommandTChan :: TChan ConnectionCommand -- ^ This `TChan` gives
                                                         --   ConnectionCommand
                                                         --   for
    } deriving (Eq)


-- TODO serviceRegistry HashMap contextId serviceContex

-- registerService ariviHandle serviceCode transportType
