-- |
-- Module      :  Arivi.P2P.RPC
-- Copyright   :
-- License     :
-- Maintainer  : 
-- Stability   :
-- Portability :
--
-- 

module Arivi.P2P.RPC
(
    request
)

where




import           Data.UUID       --toString
import           Data.UUID.V4        --nextRandom
import           Data.Maybe

import           Control.Monad
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM

import qualified Data.Map.Strict as Map
import           Arivi.P2P.Types
import           Arivi.Network.Types        (TransportType (..),NodeId)

import           Data.ByteString.Char8       (pack)

request :: ServiceCode -> ResourceID -> String -> Maybe P2PUUID ->  IO P2PUUID
request  serviceID resourceID message uuid = do
        uniqueUUID <- getUUID
        let uuid1 = fromMaybe uniqueUUID uuid
        --print ""
        -- return uuid
        outgoingMessageHandler (formServiceMessage (selfNodeId) (getPeer resourceID) resourceID  uuid1 message (checkUUID  uuid1))


{- forms the get or return type messsages from the service layer-}
formServiceMessage :: NodeId -> NodeId -> ResourceID -> P2PUUID -> String -> Bool -> P2PMessage
formServiceMessage sender receiver resourceID uuid1 message flag =
    P2PMessage {
        uuid = uuid1,
        to = receiver,
        from = sender,
        responseCode = getResponseCodeFromFlag flag,
        p2pType =  Just $ returnMessageType flag message resourceID
    }
{-returns the type based on the flag-}
returnMessageType :: Bool -> String -> ResourceID -> MessageType
returnMessageType flag resourceID message =
    case flag of
        False -> Get { resource = resourceID, serviceMessage = message }
        True -> Return { resource = resourceID, serviceMessage = message }
{-takes care of sending the p2pmessage to the network layer-}
outgoingMessageHandler :: P2PMessage -> IO P2PUUID
outgoingMessageHandler p2pMessage = do 
    -- send message (p2pMessage) 
    
    return (uuid p2pMessage)

    {-returns a peer for the particular resource id-}
getPeer :: ResourceID -> NodeId
getPeer resourceID = pack "12334556"

selfNodeId :: NodeId
selfNodeId = pack "12334556"

checkUUID ::P2PUUID -> Bool
checkUUID uuid = True

getUUID :: IO String
getUUID = toString <$> nextRandom
{-adds particular response for get or ret to be decided on response codes later-}

getResponseCodeFromFlag :: Bool -> ResponseCode
getResponseCodeFromFlag flag = 1


{-forms the option message-}

formOptionMessage :: NodeId -> P2PUUID -> P2PMessage
formOptionMessage node uuid1 = 
    P2PMessage {
        uuid = uuid1,
        to = node,
        from = selfNodeId,
        responseCode = 1,
        p2pType =  Just Options
    }

{-forms the supported message-}
formSupportedMessage :: NodeId -> P2PUUID -> Maybe MessageType -> ResponseCode -> P2PMessage
formSupportedMessage node uuid1 rpctype responsecode = 
    P2PMessage {
        uuid = uuid1,
        to = node,
        from = selfNodeId,
        responseCode = responsecode,
        p2pType = rpctype
    }
{--}

--registerResource :: ServiceCode -> [ResourceDD] -> TVar 
registerResource (y:[]) serviceID resourceListTVar = 
    atomically (modifyTVar' resourceListTVar (Map.insert y (serviceID,[])))
registerResource (y:resourceIDList) serviceID resourceListTVar = 
    do 
        atomically (modifyTVar' resourceListTVar (Map.insert y (serviceID,[])))
        registerResource resourceIDList serviceID resourceListTVar

