module Arivi.Network.Multiplexer
(
    ServiceRegistry (..)
) where

import qualified Arivi.Network.Types          as T
import           Control.Concurrent.STM.TChan (TChan)
import qualified Data.Map.Strict              as Map


newtype ServiceRegistry = ServiceRegistry {
                    serviceRegistry :: Map.Map T.ServiceCode (TChan T.PayLoad,
                                        TChan T.PayLoad )
                }






