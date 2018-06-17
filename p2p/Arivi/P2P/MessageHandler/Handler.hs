{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.MessageHandler.Handler
(

sendRequest,
readRequest

)
where
import           Data.ByteString.Char8                 as Char8 (ByteString,
                                                                 pack, unpack)
import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
                                                                toStrict)
import           Data.HashMap.Strict                   as HM
import           Data.List.Split                       (splitOn)

import           Data.Maybe
import qualified Data.UUID                             as UUID (toString)
import           Data.UUID.V4                          (nextRandom)


import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue         ()
import           Control.Concurrent.STM.TVar           ()

import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans

import           Codec.Serialise                       (deserialise, serialise)

import           Arivi.Network.Connection              ()
--import           Arivi.Network.Types                   (TransportType (..))
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv



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
sendRequest :: (HasP2PEnv m ) =>  Peer -> MessageCode -> Message -> TransportType -> m  ByteString
sendRequest peer mCode message transportType =
    do
        peerUUIDMapTVar <- getpeerUUIDMapTVarP2PEnv
        liftIO $ do
            newuuid <- getUUID
            peerUUIDMap <- readTVarIO peerUUIDMapTVar
            mvar <- newEmptyMVar ::IO (MVar P2PMessage)
            --need try  herefor formJust
            let uuidMapTVar = fromJust (HM.lookup (nodeId peer) peerUUIDMap)
            atomically (
                do
                    a <- readTVar uuidMapTVar
                    let b = HM.insert newuuid mvar a
                    writeTVar uuidMapTVar b
                )
            let
                p2pMessage = generateP2PMessage mCode message newuuid
                port = if UDP== transportType then udpPort peer else tcpPort peer
            --need try
            res <- Exception.try $ do
                let connId = openConnection (nodeId peer) (ip peer) port transportType
                sendMessage connId (Lazy.toStrict $ serialise p2pMessage)
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

readRequest :: (HasP2PEnv m ) => ConnectionInfo -> m ()
readRequest conInfo  =
    do
        peerUUIDMapTVar <- getpeerUUIDMapTVarP2PEnv
        kademTQueue <- getkademTQueueP2PEnv
        rpcTQueue <- getrpcTQueueP2PEnv
        pubsubTQueue <- getpubsubTQueueP2PEnv
        tvarConnectionInfoMap <- getConnectionInfoMapTVarP2PEnv
        liftIO $ do
            peerUUIDMap <- readTVarIO peerUUIDMapTVar
            let pNodeId = peerNodeId conInfo
                uuidMapTVar = fromJust (HM.lookup pNodeId peerUUIDMap)
                pport = port conInfo
                connId = makeConnectionId pNodeId (peerIp conInfo) pport (transportType conInfo)
            forever $ do
                -- if checkConnection conInfo then do
                    eitherByteMessage <- Exception.try  $ readMessage connId
                    case eitherByteMessage of
                        Left ( _ :: Exception.SomeException)-> return ()
                        Right byteMessage ->
                            do
                                let networkMessage = deserialise (Lazy.fromStrict byteMessage) :: P2PMessage
                                uuidMap <- atomically (readTVar uuidMapTVar)
                                let temp = HM.lookup (uuid networkMessage) uuidMap
                                if isNothing temp then
                                    do
                                        let newRequest = (uuid networkMessage, typeMessage networkMessage)
                                        case messageCode networkMessage of
                                            Kademlia -> atomically (writeTQueue kademTQueue newRequest)
                                            RPC -> atomically (writeTQueue rpcTQueue newRequest)
                                            PubSub -> atomically (writeTQueue pubsubTQueue newRequest)
                                else
                                    do
                                        let mVar = fromJust temp
                                        putMVar mVar networkMessage
                -- else




--newIncomingConnection :: (HasP2PEnv m ) => m()
--newIncomingConnection =
    --forever $ do
        {--}
{-
call getnewconnection from ANP,  this return connId
when it returns add it to PeerConnectionMapTVar with a value true
fork a readRequest on the particular connId you get from ANP
repeat
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

makeConnectionId :: NodeId
                 -> IP
                 -> Port
                 -> TransportType
                 -> ConnectionId
makeConnectionId nodeId mIpAddress mPort mTransportType =

                                            pack $  nodeId
                                                     ++ "|"
                                                     ++ mIpAddress
                                                     ++ "|"
                                                     ++ show mPort
                                                     ++ "|"
                                                     ++ show mTransportType

getPeerInfo  :: ConnectionId -> ConnectionInfo
getPeerInfo connId =

        ConnectionInfo {
            peerNodeId = read (head infolist) :: NodeId,
            peerIp = read (infolist!!1) ::IP,
            port = read (infolist!!2) ::Port,
            transportType = read (infolist!!3) ::TransportType
        }where  str = unpack connId
                infolist = splitOn "|" str



{-Dummy Functions========================================================-}

-- selfNodeId :: NodeId
-- selfNodeId = pack "12334556"

openConnection :: NodeId -> IP -> Port -> TransportType -> ConnectionId
openConnection nodeId ip port transportType = pack "892sadasd346384"

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()

readMessage :: ConnectionId -> IO ByteString
readMessage connId =
    Lazy.toStrict . serialise . generateP2PMessage Kademlia (pack "892sadasd346384") <$> getUUID
