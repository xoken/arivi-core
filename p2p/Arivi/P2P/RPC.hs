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




import           Data.ByteString.Char8       (pack)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad

import           Arivi.Network.Types         (NodeId, TransportType (..))
import           Arivi.P2P.MessageHandler
import           Arivi.P2P.Types



type ServiceID = String

request :: ServiceID -> ResourceID -> String -> Maybe P2PUUID ->  IO P2PUUID
request  serviceID resourceID message uuid = do
        uniqueUUID <- getUUID
        let uuid1 = fromMaybe uniqueUUID uuid
        --print ""
        -- return uuid
        outgoingMessageHandler (formServiceMessage selfNodeId (getPeer resourceID) resourceID  uuid1 message (checkUUID  uuid1))



{-takes care of sending the p2pmessage to the network layer-}
outgoingMessageHandler :: P2PMessage -> IO P2PUUID
outgoingMessageHandler p2pMessage =
    -- send message (p2pMessage)

    return (uuid p2pMessage)

        {-returns a peer for the particular resource id-}
--getPeer :: ResourceID -> TVar -> NodeId
getPeer :: ResourceID -> NodeId
getPeer resourceID = pack "12334556"
{-getpeerID will replace getPeer once its completely written for now getPeer acts as the dummy-}
getPeerID :: ResourceID -> TVar ResourceList -> IO ()
getPeerID resourceID resourceListTVar =
            atomically (
                do
                    a <- readTVar resourceListTVar

                    let c = Map.lookup resourceID a
                        b = fromJust c
                        x = snd b
                    case x of
                        y:ys ->
                            do
                                return y
                                let d = (fst b, ys)
                                    e = Map.insert resourceID d a
                                writeTVar resourceListTVar e
                    --    [] -> getOptionForResourceID
                    -- needs to be written for this
                )



--registerResource :: ServiceCode -> [ResourceDD] -> TVar
registerResource [y] serviceID resourceListTVar =
    atomically (modifyTVar' resourceListTVar (Map.insert y (serviceID,[])))
registerResource (y:resourceIDList) serviceID resourceListTVar =
    do
        atomically (modifyTVar' resourceListTVar (Map.insert y (serviceID,[])))
        registerResource resourceIDList serviceID resourceListTVar


{-readSupported p2pMessage resouceListTVar  =
    addToResourceListTVar (resourceList fromMaybe p2pType p2pMessage) (from p2pMessage) resourceListTVar
-}

addToResourceListTVar :: [ResourceID] -> Peer -> TVar ResourceList-> IO ()
addToResourceListTVar [] from resourceListTVar= return ()

addToResourceListTVar (x:resourceList) from resourceListTVar =
    do
        let f (c,d) = (c, d ++ [from])
        atomically (
            do
                a <- readTVar resourceListTVar
                let c = Map.lookup x a
                    b = fromJust c
                    v = snd b
                    y = v ++ [from]
                    z = (fst b, y)
                    e = Map.insert x z a
                writeTVar resourceListTVar e
            )
        addToResourceListTVar resourceList from resourceListTVar

{-fillPeers resourceListTVar resourceID =
    do
        let x = []--getnewPeers -}
