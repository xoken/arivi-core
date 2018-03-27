module Network.Arivi.Multiplexer
(
Registry (..)
) where 
    
import qualified Data.Map.Strict                    as Map 
import qualified Network.Arivi.Types                as T 


data Registry = Registry {
                    registry :: Map.Map Int (T.PayLoad -> IO ())
                } 





-- ariviConfig   = AriviConifg x y z 
-- ariviInstance = getAriviInstance ariviConfig 

-- register ariviInstance (0,messageHandler)
-- register ariviInstance (1,messageHandler)
-- register ariviInstance (2,messageHandler)

-- runAriviInstance ariviInstance 