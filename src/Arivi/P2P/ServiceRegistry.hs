module Arivi.P2P.ServiceRegistry
(
    ConnectionCommand(..),
    ContextId,
    ServiceContext(..)
) where

import           Arivi.P2P.Connection (Connection, ConnectionId)
import           Arivi.Network.Types             (ServiceCode, TransportType)
import           Control.Concurrent.MVar         (MVar)
import           Control.Concurrent.STM.TChan
import           Data.HashMap.Strict             (HashMap)
import           Data.UUID                       (UUID)

type ContextId = UUID

data ConnectionCommand = ConnectionCommand {
                             connectionId   :: ConnectionId
                            ,connectionMVar :: MVar Connection
                        }


data ServiceContext = ServiceContext {
           contextId              :: ContextId
          ,serviceCode            :: ServiceCode
          ,transportType          :: TransportType
          ,outputTChan            :: TChan String
          ,connectionCommandTChan :: TChan ConnectionCommand
        } deriving (Eq)


-- TODO serviceRegistry HashMap contextId serviceContex

-- registerService ariviHandle serviceCode transportType
