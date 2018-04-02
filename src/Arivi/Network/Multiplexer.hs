module Arivi.Network.Multiplexer
(
Registry (..)
) where

import qualified Data.Map.Strict                    as Map
import qualified Arivi.Network.Types                as T


newtype Registry = Registry {
                    registry :: Map.Map Int (T.PayLoad -> IO ())
                }





-- ariviConfig   = AriviConifg x y z
-- ariviInstance = getAriviInstance ariviConfig

-- register ariviInstance (0,messageHandler)
-- register ariviInstance (1,messageHandler)
-- register ariviInstance (2,messageHandler)

-- runAriviInstance ariviInstance
