module Arivi.P2P.RPC.Function
(
{-
sendRequest ()
-}
)
where

import Arivi.P2P.MessageHandler.Functions
import Arivi.P2P.MessageHandler.Types (RPCTChan)
import Arivi.P2P.RPC.Types



{-
--getResource(resourceID serviceMessage)
-- form messageType
--getPeer from ResourcetoPeerList
--ret = sendRequest peer messageType Tcp
-- check ret format with try catch
--if not good then return getResource(resourceID serviceMessage)
-- else return (serviceMessage ret)
-}

{-
acceptRequest (ResourceID)
--read from ResourceID Tchan
return (serviceMessage,
-}
