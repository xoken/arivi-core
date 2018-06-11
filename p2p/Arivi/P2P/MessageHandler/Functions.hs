{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.MessageHandler.Functions
(

sendRequest

)
where
import           Data.ByteString.Char8          as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy           as ByteStringLazy (toStrict)
import           Data.HashMap.Strict            as HM

import           Data.Maybe
import qualified Data.UUID                      as UUID (toString)
import           Data.UUID.V4                   (nextRandom)


import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar    ()
import qualified Control.Exception.Lifted       as Exception (SomeException,
                                                              try)
import           Control.Monad                  ()

import           Codec.Serialise                (serialise)

import           Arivi.Network.Connection       ()
import           Arivi.Network.Types            (ConnectionId, NodeId,
                                                 TransportType (..))
import           Arivi.P2P.MessageHandler.Types



{-exposed functions-}
{-
sendRequest(Peer, MessageType, TransportType)
-- blocking function
-- generate P2PMessage from Peer and MessageType
--generate uuid and MVar
-- enter uuid->MVar in PeerToUUIDMap
-- open conn
-- sendmessageimport           Data.ByteString.Char8       as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy        as ByteStringLazy (ByteString,
                                                                toStrict)
--async race between takeMVar (blocking) and Timer
--left -> return MessageType from MVar
--right -> return blank messageType
-- delete uuid from PeerToUUIDMap
--make use of exceptions for checking the format of p2pmessage if left
    catch them in this function and return blank messageType
-}
sendRequest :: TVar PeerUUIDMap -> Peer -> MessageCode -> Message -> TransportType -> IO ByteString
sendRequest peerUUIDMapTVar peer mCode message transportType =
    do
        --peerUUIDMapTVar <- getpeerUUIDMapTVar
        newuuid <- getUUID
        peerUUIDMap <- readTVarIO peerUUIDMapTVar
        mvar <- newEmptyMVar ::IO (MVar P2PMessage)
        --need try  herefor formJust
        let uuidMapTVar = fromJust (HM.lookup peer peerUUIDMap)
        atomically (
            do
                a <- readTVar uuidMapTVar
                let b = HM.insert newuuid mvar a
                writeTVar uuidMapTVar b
            )
        let
            p2pMessage = generateP2PMessage mCode message newuuid
            port = if UDP== transportType then peerUDPPort peer else peerTCPPort peer
        --need try
        res <- Exception.try $ do
            let connId = openConnection (peerNodeId peer) (peerIp peer) port transportType
            sendMessage connId (ByteStringLazy.toStrict $ serialise p2pMessage)
        case res of
            Left( _ :: Exception.SomeException)-> return $ pack "000000"
            Right _ ->
                do
                    p2pReturnMessage <- takeMVar mvar  :: IO P2PMessage
                    atomically (
                        do
                            a <- readTVar uuidMapTVar
                            let b = HM.delete newuuid a
                            writeTVar uuidMapTVar b
                        )
                    let returnMessage = typeMessage p2pReturnMessage
                    return returnMessage

        -- return $ pack "000000"




{-
readRequest (connid)
-- fork
--do forever
try
--p2pMEssage = readmessage (connid) {ANP, blocking}
catch exceptions thrown by ANP
if conn terminated end
else
--make use of exceptions for checking the format of p2pmessage if left
--catch them in this function and {update in peer rep table} restart loop
--check uuid p2pMessage is in PeerToUUIDMap
-- if yes then put in its MVar the messagetype p2pmessage
--else check messagetype
        case kadem -> put it in kademTChan
        case RPC -> put it in RPCTChan
        case PubSub -> put it in PubSubTChan
-}
{-
readRequest :: TVar PeerUUIDMap ->  TQueue MessageInfo ->  TQueue MessageInfo ->  TQueue MessageInfo -> Peer -> IO()
readRequest peerUUIDMapTVar kademTQueue rpcTQueue pubsubTQueue peer =
    do
        res <- Exception.try $ do
            let connId = openConnection (peerNodeId peer) (peerIp peer) port transportType
            sendMessage connId (ByteStringLazy.toStrict $ serialise p2pMessage)
        case res of
            Left( _ :: Exception.SomeException)-> return ()
            Right _ ->
                do
-}


{-Support Functions===========================================================-}
generateP2PMessage :: MessageCode -> Message -> P2PUUID -> P2PMessage
generateP2PMessage mCode message1 uuid1 =
    P2PMessage {
        uuid = uuid1,
        messageCode =  mCode,
        typeMessage = message1
    }

getUUID :: IO P2PUUID
getUUID = UUID.toString <$> nextRandom

{-Dummy Functions========================================================-}

selfNodeId :: NodeId
selfNodeId = pack "12334556"

openConnection :: NodeId -> IP -> Port -> TransportType -> ConnectionId
openConnection nodeId ip port transportType = pack "892sadasd346384"

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()
