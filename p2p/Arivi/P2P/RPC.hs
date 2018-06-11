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

)

where




import           Data.ByteString.Char8       (pack)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.MVar
import           Control.Monad

import           Arivi.Network.Types         (NodeId, TransportType (..))
import           Arivi.P2P.MessageHandler
import           Arivi.P2P.Types



type ServiceID = String

{-
        {-
getResource :: ResourceID -> String ->
getResource resourceID message =
    do
        --1 getPeer
        peer <- getPeerID resourceID
        --2 send message
        uuid <- sendGetMessage peer resourceID message
        --3 create mvar
        mvar <- newEmptyMVar
        --4 enter uuid in UUIDList with mvar
        atomically (
            do
                a <- readTVar UUIDListTVar
                let b = Map.insert uuid mvar
                writeTVar UUIDListTVar b
        )
        --5 read on mvar
        p2pMessage <- takeMVar mvar
        atomically (
            do
                a <- readTVar UUIDListTVar
                let b = Map.delete uuid
                writeTVar UUIDListTVar b
        )
        --6 check to,from, messagetype and responsecode
        let tmp = checkReturnMessage p2pMessage resourceID
        --7 accordingly go to 1 and start again or return message
        if tmp then
            return serviceMessage fromJust p2pType p2pMessage else
            getResource resourceID message
-}


-- checkReturnMessage :: P2PMessage -> ResourceID -> Bool
-- checkReturnMessage p2pMessage resourceID = True
--     {-do
--         let self = to p2pMessage
--             peerId = from p2pMessage
--             rCode = responseCode p2pMessage
--             -}




{- getpeerID will replace getPeer once its completely written for now getPeer acts as the dummy-}
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

uuidCheckThread :: TVar UUIDTime -> IO ()
uuidCheckThread uuidTimeTVar  = forever $ do
    a <- readTVarIO uuidTimeTVar
    foldlWithKey checkUUID [] a

{-
    let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
    foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
    -}

checkUUID :: [P2PUUID] -> P2PUUID -> Time -> [P2PUUID]
checkUUID result uuid time =
    if (timeChecker time) then
        do
            let tmp = unblockUUID uuid
            if tmp then
                return (result ++ [uuid])
            else
                return result
    else
        result

unblockUUID :: P2PUUID -> Bool
unblockUUID uuid =
    do
        a <- atomically readTVar uuidListTVar
        b <- Map.lookup uuid uuidListTVar
        if (isNothing b) then
            deleteExpiredUUID uuid uuidTimeTVar
            return ()
        else
            tmp <- tryPutMVar (nullP2PMessage uuid)
            return tmp


deleteExpiredUUID uuid uuidTimeTVar =


timeChecker :: UTCTime -> Bool
timeChecker time =
    do
        curr <- getCurrentTime
        let nom = diffUTCTime curr time
            sec = picosecondsToDiffTime nominalDiffTimeToSeconds nom
            rtt = secondsToDiffTime 30
        return (sec > rtt) &&  True


{- helper dummy functions===========================================================================================================-}

nullP2PMessage uuid1 =
    P2PMessage {
        uuid = uuid1,
        to = selfNodeId,
        from = selfNodeId,
        responseCode = 99,
        p2pType =  Nothing
    }



{- redundant functions==============================================================================================================-}



{-fillPeers resourceListTVar resourceID =
    do
        let x = []--getnewPeers
        -}
{-takes care of sending the p2pmessage to the network layer-}
outgoingMessageHandler :: P2PMessage -> IO P2PUUID
outgoingMessageHandler p2pMessage =
    -- send message (p2pMessage)

    return (uuid p2pMessage)

        {-returns a peer for the particular resource id-}


request :: ServiceID -> ResourceID -> String -> Maybe P2PUUID ->  IO P2PUUID
request  serviceID resourceID message uuid = do
        uniqueUUID <- getUUID
        let uuid1 = fromMaybe uniqueUUID uuid
        --print ""
        -- return uuid
        outgoingMessageHandler (formServiceMessage selfNodeId (getPeer resourceID) resourceID  uuid1 message (checkUUID  uuid1))

        --getPeer :: ResourceID -> TVar -> NodeId
getPeer :: ResourceID -> NodeId
getPeer resourceID = pack "12334556"
-}