module Arivi.P2P.Rpc where

import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.Types
import Control.Concurrent.STM
import Data.ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

initRPC :: Map k (Handler msg IO) -> Map NodeId [PeerDetails] -> IO ()
initRPC handlers peers = return ()
