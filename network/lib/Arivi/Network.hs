module Arivi.Network
  ( ConnectionHandle (..)
  , openConnection
  , runServer
  , HasSecretKey (..)
  , HasLogging   (..)
  )
where

import Network.Socket (ServiceName)

import Arivi.Env
import Arivi.Logging (HasLogging (..))
import Arivi.Network.Instance (openConnection)
import Arivi.Network.StreamServer (runTCPServer)
import Arivi.Network.Types (ConnectionHandle (..))

runServer ::
       (HasSecretKey m, HasLogging m)
    => (ServiceName, ServiceName)
    -> (ConnectionHandle -> m ())
    -> m ()
runServer (tcpPort, _) = runTCPServer tcpPort
