module Arivi.Network.Multiplexer
(
    ServiceRegistry (..)
) where

import qualified Arivi.Network.Types as T
import qualified Data.Map.Strict     as Map


newtype ServiceRegistry = ServiceRegistry {
                    serviceRegistry :: Map.Map T.ServiceId (T.PayLoad ->IO ())
                }


