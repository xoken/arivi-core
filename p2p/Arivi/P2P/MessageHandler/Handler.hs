{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.MessageHandler.Handler
(

sendRequest,
sendResponse,
readKademliaRequest,
readRPCRequest,
readPubSubRequest,
newIncomingConnection

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

import           Control.Concurrent                    (threadDelay)
import qualified Control.Concurrent.Async              as Async (async, race)
import           Control.Concurrent.Lifted             (fork)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue         ()
import           Control.Concurrent.STM.TVar           ()

import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever, unless, when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans

import           Codec.Serialise                       (deserialise, serialise)

--import           Arivi.Network.Types                   (TransportType (..))
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.Utils.Exception


{-exposed functions-}
{-
sendRequest(Peer, MessageType, TransportType)
-- blocking function
-- generate P2PMessage from Peer and MessageType
--generate uuid and MVar
-- enter uuid->MVar in PeerToUUIDMap
-- open conn
-- sendmessagei
--async race between takeMVar (blocking) and Timer
--left -> return MessageType from MVar
--right -> return blank messageType
-- delete uuid from PeerToUUIDMap
--
-}
sendRequest :: (HasP2PEnv m ) =>  Peer -> MessageCode -> Message -> TransportType -> m  ByteString
sendRequest peer mCode message transportType =
    do
        peerUUIDMapTVar <- getpeerUUIDMapTVarP2PEnv
        newuuid <-  liftIO $ getUUID

        mvar <-  liftIO $  newEmptyMVar
        --need to do something about fromJust error
        liftIO $ atomically( do
            peerUUIDMap <-  readTVar peerUUIDMapTVar
            let tempTVar =HM.lookup (nodeId peer) peerUUIDMap
            when (isNothing tempTVar) $  do
                newuuidMapTVar <-  newTVar HM.empty
                let newpeerUUIDMap = HM.insert (nodeId peer) newuuidMapTVar peerUUIDMap
                writeTVar peerUUIDMapTVar newpeerUUIDMap
            )
        finalpeerUUIDMap <-  liftIO $ readTVarIO peerUUIDMapTVar
        let uuidMapTVar = fromJust (HM.lookup (nodeId peer) finalpeerUUIDMap)
        liftIO $ atomically (
            do
                a <- readTVar uuidMapTVar
                let b = HM.insert newuuid mvar a
                writeTVar uuidMapTVar b
            )
        let p2pMessage = generateP2PMessage mCode message newuuid
            conInfo = getConnectionInfo peer transportType
            connId = makeConnectionId (nodeId peer) (peerIp conInfo) (port conInfo) transportType
        tmp <- checkConnection conInfo
        unless tmp $ do
            res1 <- liftIO $ Exception.try $
                openConnection (nodeId peer) (ip peer) (port conInfo) transportType
            case res1 of
                Left ( e :: Exception.SomeException)-> throw e
                Right netConnId ->do
                    unless (netConnId==connId) $ throw HandlerOpenConnectionError
                    addConnection conInfo

        res <-  liftIO $ Exception.try $
            sendMessage connId (Lazy.toStrict $ serialise p2pMessage)
        case res of
            Left( _ :: Exception.SomeException)-> throw HandlerSendMessageTimeout
            Right _ ->
                do
                    winner <-  liftIO $ Async.race (threadDelay 30000000) (takeMVar mvar  :: IO P2PMessage)
                    case winner of
                        Left _ -> return $ throw HandlerSendMessageTimeout
                        Right p2pReturnMessage -> do
                            liftIO $ atomically (
                                do
                                    a <- readTVar uuidMapTVar
                                    let b = HM.delete newuuid a
                                    writeTVar uuidMapTVar b
                                )
                            let returnMessage = typeMessage p2pReturnMessage
                            return   returnMessage

        -- return $ pack "000000"


sendResponse :: (HasP2PEnv m ) => MessageInfo -> Peer -> TransportType -> MessageCode -> m()
sendResponse messageInfo peer transportType mCode=
    do
        let p2pMessage = generateP2PMessage mCode (snd messageInfo) (fst messageInfo)
            conInfo  = getConnectionInfo peer transportType
            connId = makeConnectionId (nodeId peer) (peerIp conInfo) (port conInfo) transportType
        tmp <- checkConnection conInfo
        if tmp then do
            res <-  liftIO $ Exception.try $
                sendMessage connId (Lazy.toStrict $ serialise p2pMessage)
            case res of
                Left( _ :: Exception.SomeException)-> throw HandlerSendMessageTimeout
                Right _ -> return ()
        else
            throw HandlerConnectionBroken

readKademliaRequest :: (HasP2PEnv m ) => m MessageInfo
readKademliaRequest = do
    kademTQueue <- getkademTQueueP2PEnv
    messInfo <- liftIO $ atomically $ readTQueue kademTQueue
    return messInfo

readRPCRequest :: (HasP2PEnv m ) => m MessageInfo
readRPCRequest = do
    rpcTQueue <- getrpcTQueueP2PEnv
    messInfo <- liftIO $ atomically $ readTQueue rpcTQueue
    return messInfo

readPubSubRequest :: (HasP2PEnv m ) => m MessageInfo
readPubSubRequest = do
    pubsubTQueue <- getpubsubTQueueP2PEnv
    messInfo <- liftIO $ atomically $ readTQueue pubsubTQueue
    return messInfo
{-
readRequest (connid)
-- fork
--do forever
try
--p2pMEssage = readmessage (connid) {ANP, blocking}
catch exceptions thrown by ANP
if conn terminated endmCode
        forkIO (readRequest connectionInfo)
else
        forkIO (readRequest connectionInfo)
--make use of exceptions for checking the format of p2pmessage if left
        forkIO (readRequest connectionInfo)
--catch them in this function and {update in peer rep table} restart loop
        forkIO (readRequest connectionInfo)
--check uuid p2pMessage is in PeerToUUIDMap
        forkIO (readRequest connectionInfo)
-- if yes then put in its MVar the messagetype p2pmessage
        forkIO (readRequest connectionInfo)
--else check messagetype
        forkIO (readRequest connectionInfo)
        case kadem -> put it in kademTChan
        forkIO (readRequest connectionInfo)
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
        peerUUIDMap <-  liftIO $ readTVarIO peerUUIDMapTVar
        let pNodeId = peerNodeId conInfo
            uuidMapTVar = fromJust (HM.lookup pNodeId peerUUIDMap)
            pport = port conInfo
            connId = makeConnectionId pNodeId (peerIp conInfo) pport (transportType conInfo)
        tmp <- checkConnection conInfo
        when tmp $
            do
                eitherByteMessage <-  liftIO $ Exception.try  $ readMessage connId
                case eitherByteMessage of
                    Left ( _ :: Exception.SomeException)-> return ()
                    Right byteMessage ->
                        do
                            let networkMessage = (deserialise (Lazy.fromStrict byteMessage)) :: P2PMessage
                            uuidMap <-  liftIO $ atomically (readTVar uuidMapTVar)
                            let temp = HM.lookup (uuid networkMessage) uuidMap
                            if isNothing temp then
                                do
                                    let newRequest = (uuid networkMessage, typeMessage networkMessage)
                                    case messageCode networkMessage of
                                        Kademlia ->  liftIO $ atomically (writeTQueue kademTQueue newRequest)
                                        RPC ->  liftIO $ atomically (writeTQueue rpcTQueue newRequest)
                                        PubSub ->  liftIO $ atomically (writeTQueue pubsubTQueue newRequest)
                            else
                                do
                                    let mVar = fromJust temp
                                    liftIO $ putMVar mVar networkMessage
                readRequest conInfo

newIncomingConnection :: (HasP2PEnv m ) => m()
newIncomingConnection =
    do
        connectionId <- liftIO $ getNewConnection
        let conInfo = getPeerInfo connectionId
        addConnection conInfo
        fork (readRequest conInfo)
        newIncomingConnection



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

getConnectionInfo :: Peer -> TransportType -> ConnectionInfo
getConnectionInfo peer transportType1 =
    ConnectionInfo{
        peerNodeId = nodeId peer,
        peerIp = ip peer,
        port = pport,
        transportType =transportType1
    }where  pport = if transportType1==UDP then udpPort peer else tcpPort peer

getPeerInfo  ::  ConnectionId -> ConnectionInfo
getPeerInfo connId =

        ConnectionInfo {
            peerNodeId = read (head infolist) :: NodeId,
            peerIp = read (infolist!!1) ::IP,
            port = read (infolist!!2) ::Port,
            transportType = read (infolist!!3) ::TransportType
        }where  str = unpack connId
                infolist = splitOn "|" str


checkConnection :: (HasP2PEnv m ) => ConnectionInfo -> m Bool
checkConnection conInfo = do
    connectionInfoMapTVar <- getConnectionInfoMapTVarP2PEnv
    connectionInfoMap <- liftIO $ readTVarIO connectionInfoMapTVar
    if isNothing (HM.lookup conInfo connectionInfoMap) then
        return False
    else
        return True

addConnection :: (HasP2PEnv m ) => ConnectionInfo -> m ()
addConnection conInfo = do
    connectionInfoMapTVar <- getConnectionInfoMapTVarP2PEnv
    liftIO $ atomically (do
        connectionInfoMap <- readTVar connectionInfoMapTVar
        let newConnectionInfoMap = HM.insert conInfo True connectionInfoMap
        writeTVar connectionInfoMapTVar newConnectionInfoMap
        )
    return()



{-Dummy Functions========================================================-}

-- selfNodeId :: NodeId
-- selfNodeId = pack "12334556"
getNewConnection :: IO ConnectionId
getNewConnection = do
    let connId = makeConnectionId "dvz" "jbf" 6587 UDP
    return connId


openConnection :: NodeId -> IP -> Port -> TransportType -> IO ConnectionId
openConnection nodeId ip port transportType = return (pack "892sadasd346384")

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()

readMessage :: ConnectionId -> IO ByteString
readMessage connId =
    Lazy.toStrict . serialise . generateP2PMessage Kademlia (pack "892sadasd346384") <$> getUUID
