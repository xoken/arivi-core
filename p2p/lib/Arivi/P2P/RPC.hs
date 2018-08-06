module Arivi.P2P.Rpc where

import Arivi.P2P.Types
import Arivi.P2P.MessageHandler.HandlerTypes

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.ByteString
import Control.Concurrent.STM

initRPC :: Map k (Handler msg IO) -> Map NodeId ([PeerDetails]) -> IO ()
initRPC handlers peers = do
  return ()
