{-# LANGUAGE ScopedTypeVariables #-}

module Arivi.P2P.MessageHandler.Handler
    ( sendRequest
    , sendResponse
    , readKademliaRequest
    , readRPCRequest
    , readPubSubRequest
    , sendRequestforKademlia
    -- , newIncomingConnection
    ) where

import           Data.ByteString.Char8                 as Char8 (ByteString,
                                                                 pack, unpack)
import qualified Data.ByteString.Lazy                  as Lazy (fromStrict,
                                                                toStrict)
import           Data.HashMap.Strict                   as HM
import           Data.List.Split                       (splitOn)

import           Data.Maybe
import qualified Data.UUID                             as UUID (toString)
import           Data.UUID.V4                          (nextRandom)

import           Control.Concurrent                    (forkIO)
import qualified Control.Concurrent.Async              as Async (async, race)
import           Control.Concurrent.Lifted             (fork, threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue         ()
import           Control.Concurrent.STM.TVar           ()
import           Control.Exception                     (throw)

import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (forever, unless, when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans

import           Codec.Serialise                       (deserialise, serialise)

import           Arivi.Network.Connection              ()

--import           Arivi.Network.Types                   (TransportType (..))
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.Utils.Exception

-- | used by RPC and PubSub to send outgoing requests. This is a blocing call which returns the reply
sendRequest ::
       (HasP2PEnv m) => NodeId -> MessageType -> P2PPayload -> m P2PPayload
sendRequest node mType p2pPayload = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    newuuid <- liftIO getUUID
    mvar <- liftIO newEmptyMVar
    connHandle <- liftIO $ getConnHandleFromNodeID node nodeIdMapTVar mType
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup node nodeIdMap)
    peerDetails <- liftIO $ readTVarIO peerDetailsTVar
    let uuidMapTVar = tvarUUIDMap peerDetails
    liftIO $
        atomically
            (do uuidMap <- readTVar uuidMapTVar
                let newUUIDMAP = HM.insert newuuid mvar uuidMap
                writeTVar uuidMapTVar newUUIDMAP)
    let p2pMessage = generateP2PMessage mType p2pPayload newuuid
    res <-
        liftIO $
        Exception.try $
        sendMessage connHandle (Lazy.toStrict $ serialise p2pMessage)
    case res of
        Left (e :: Exception.SomeException) -> do
            liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
            throw e
        Right _ -> do
            winner <-
                liftIO $
                Async.race
                    (threadDelay 30000000)
                    (takeMVar mvar :: IO P2PMessage)
            case winner of
                Left _ -> do
                    liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
                    throw HandlerSendMessageTimeout
                Right (p2pReturnMessage :: P2PMessage) -> do
                    liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
                    let returnMessage = payload p2pReturnMessage
                    return returnMessage

sendRequestforKademlia ::
       (HasP2PEnv m)
    => NodeId
    -> MessageType
    -> P2PPayload
    -> Port
    -> IP
    -> m P2PPayload
sendRequestforKademlia node mType p2pPayload port ip = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    let maybePeer = HM.lookup node nodeIdMap
    if isNothing maybePeer -- concurrency issues might arise here need to check
        then do
            res <- liftIO $ Exception.try $ openConnection node ip port UDP
            case res of
                Left (e :: Exception.SomeException) -> throw e
                Right connHandle -> do
                    liftIO $
                        addPeerFromConnection node UDP connHandle nodeIdMapTVar
                    sendRequest node mType p2pPayload
        else sendRequest node mType p2pPayload

-- | This is used by Kademlia, RPC and PubSub to send back resonses to incoming requests
sendResponse :: (HasP2PEnv m) => NodeId -> MessageInfo -> MessageType -> m ()
sendResponse node messageInfo mType = do
    let p2pMessage =
            generateP2PMessage mType (snd messageInfo) (fst messageInfo)
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    connHandle <- liftIO $ getConnHandleFromNodeID node nodeIdMapTVar mType
    res <-
        liftIO $
        Exception.try $
        sendMessage connHandle (Lazy.toStrict $ serialise p2pMessage)
    case res of
        Left (e :: Exception.SomeException) -> throw e
        Right _                             -> return ()

-- | This is used by Kademlia to read incoming requests
readKademliaRequest :: (HasP2PEnv m) => m MessageInfo
readKademliaRequest = do
    kademTQueue <- getkademTQueueP2PEnv
    liftIO $ atomically $ readTQueue kademTQueue

-- | This is used by RPC to read incoming requests
readRPCRequest :: (HasP2PEnv m) => m MessageInfo
readRPCRequest = do
    rpcTQueue <- getrpcTQueueP2PEnv
    liftIO $ atomically $ readTQueue rpcTQueue

-- | This is used by PubSub to read incoming requests
readPubSubRequest :: (HasP2PEnv m) => m MessageInfo
readPubSubRequest = do
    pubsubTQueue <- getpubsubTQueueP2PEnv
    liftIO $ atomically $ readTQueue pubsubTQueue

-- | This spawns off a thread for the connection handle specified by nodeid and transporttype and manages the incoming messages by either matching them to the uuid or depositing it in respective mvar
readRequest :: (HasP2PEnv m) => NodeId -> TransportType -> m ()
readRequest node transportType = do
    kademTQueue <- getkademTQueueP2PEnv
    rpcTQueue <- getrpcTQueueP2PEnv
    pubsubTQueue <- getpubsubTQueueP2PEnv
    nodeIdPeerMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ readTVarIO nodeIdPeerMapTVar
    let peerDetailsTVar = fromJust (HM.lookup node nodeIdPeerMap) -- not possible that node isnt here but need to try
    connHandle <- liftIO $ getConnectionHandle peerDetailsTVar transportType
    peerDetails <- liftIO $ readTVarIO peerDetailsTVar
    let uuidMapTVar = tvarUUIDMap peerDetails
    tmp <-
        liftIO $
        forkIO $
        readRequestThread
            connHandle
            uuidMapTVar
            kademTQueue
            rpcTQueue
            pubsubTQueue
    return ()

readRequestThread ::
       ConnectionId
    -> TVar UUIDMap
    -> TQueue MessageInfo
    -> TQueue MessageInfo
    -> TQueue MessageInfo
    -> IO ()
readRequestThread connHandle uuidMapTVar kademTQueue rpcTQueue pubsubTQueue = do
    eitherByteMessage <- Exception.try $ readMessage connHandle
    case eitherByteMessage of
        Left (_ :: Exception.SomeException) -> return ()
        Right byteMessage -> do
            let networkMessage =
                    deserialise (Lazy.fromStrict byteMessage) :: P2PMessage
            uuidMap <- atomically (readTVar uuidMapTVar)
            let temp = HM.lookup (uuid networkMessage) uuidMap
            if isNothing temp
                then do
                    let newRequest =
                            (uuid networkMessage, payload networkMessage)
                    case messageType networkMessage of
                        Kademlia ->
                            atomically (writeTQueue kademTQueue newRequest)
                        RPC -> atomically (writeTQueue rpcTQueue newRequest)
                        PubSub ->
                            atomically (writeTQueue pubsubTQueue newRequest)
                else do
                    let mVar = fromJust temp
                    putMVar mVar networkMessage
            readRequestThread
                connHandle
                uuidMapTVar
                kademTQueue
                rpcTQueue
                pubsubTQueue

-- newIncomingConnection :: (HasP2PEnv m) => m ()
-- newIncomingConnection = do
--     (nodeId, connHandle, transportType) <- liftIO getNewConnection
--     addPeerfromConnection nodeId connHandle transportType
--     fork (readRequest conInfo)
--     newIncomingConnection
{-Support Functions===========================================================-}
-- | atomically checks for existing handle which is returned if it exists or else its status is changed to pending. then a new connection is established and it is stored as well as returned.
--
getConnectionHandle :: TVar PeerDetails -> TransportType -> IO ConnectionId
getConnectionHandle peerDetailsTVar transportType = do
    peerDetails <- readTVarIO peerDetailsTVar
    let connMaybe =
            if transportType == TCP
                then streamHandle peerDetails
                else datagramHandle peerDetails
    case connMaybe of
        NotConnected -> do
            check <-
                atomically
                    (changeConnectionStatus peerDetailsTVar transportType)
            if check
                then do
                    res <-
                        Exception.try $
                        openConnection
                            (nodeId peerDetails)
                            (fromJust (ip peerDetails))
                            (if transportType == TCP
                                 then fromJust (tcpPort peerDetails)
                                 else fromJust (udpPort peerDetails))
                            transportType
                    case res of
                        Left (e :: Exception.SomeException) -> throw e
                        Right connHandle -> do
                            atomically
                                (do oldPeerDetails <- readTVar peerDetailsTVar
                                    let newPeerDetails =
                                            if transportType == TCP
                                                then oldPeerDetails
                                                         { streamHandle =
                                                               Connected
                                                                   { connId =
                                                                         connHandle
                                                                   }
                                                         }
                                                else oldPeerDetails
                                                         { datagramHandle =
                                                               Connected
                                                                   { connId =
                                                                         connHandle
                                                                   }
                                                         }
                                    writeTVar peerDetailsTVar newPeerDetails)
                            return connHandle
                else getConnectionHandle peerDetailsTVar transportType
        Pending -> do
            threadDelay 3000 --should depend on avg time to open connection
            getConnectionHandle peerDetailsTVar transportType
        Connected connHandle -> return connHandle

-- | if connhandle is NotConnected then change it to Pending. Should be done atomically
changeConnectionStatus :: TVar PeerDetails -> TransportType -> STM Bool
changeConnectionStatus peerDetailsTVar transportType = do
    peerDetails <- readTVar peerDetailsTVar
    let connCheck =
            if transportType == TCP
                then streamHandle peerDetails
                else datagramHandle peerDetails
    if connCheck == NotConnected
        then do
            let newPeerDetails =
                    if transportType == TCP
                        then peerDetails {streamHandle = Pending}
                        else peerDetails {datagramHandle = Pending}
            writeTVar peerDetailsTVar newPeerDetails
            return True
        else return False

-- | delete an uuid entry from the map
deleteUUID :: P2PUUID -> TVar UUIDMap -> STM ()
deleteUUID uuid uuidMapTVar = do
    a <- readTVar uuidMapTVar
    let b = HM.delete uuid a
    writeTVar uuidMapTVar b

-- | get connection handle for the specific nodeID and mesaage type from the hashmap
getConnHandleFromNodeID ::
       NodeId -> TVar NodeIdPeerMap -> MessageType -> IO ConnectionId
getConnHandleFromNodeID node nodeIdMapTVar mType = do
    nodeIdMap <- readTVarIO nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup node nodeIdMap)
    getConnectionHandle
        peerDetailsTVar
        (if mType == RPC
             then TCP
             else UDP)

-- | wraps the payload with message type { Kademlia | RPC | PubSub} and UUID
generateP2PMessage :: MessageType -> P2PPayload -> P2PUUID -> P2PMessage
generateP2PMessage mType message uuid1 =
    P2PMessage {uuid = uuid1, messageType = mType, payload = message}

getUUID :: IO P2PUUID
getUUID = UUID.toString <$> nextRandom

-- | function for adding peer from a particular connectionhandle
addPeerFromConnection ::
       NodeId -> TransportType -> ConnectionId -> TVar NodeIdPeerMap -> IO ()
addPeerFromConnection node transportType connHandle nodeIdPeerMapTVar = do
    uuidMapTVar <- newTVarIO HM.empty
    atomically
        (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
            let mapEntry = HM.lookup node nodeIdPeerMap
            peerDetails <-
                maybe
                    (do let newDetails =
                                PeerDetails
                                    { nodeId = node
                                    , rep = Nothing
                                    , ip = Nothing
                                    , udpPort = Nothing
                                    , tcpPort = Nothing
                                    , streamHandle = NotConnected
                                    , datagramHandle = NotConnected
                                    , tvarUUIDMap = uuidMapTVar
                                    }
                        peerTVar <- newTVar newDetails
                        readTVar peerTVar)
                    readTVar
                    mapEntry
            let newPeerDetails =
                    if transportType == TCP
                        then peerDetails
                                 { streamHandle =
                                       Connected {connId = connHandle}
                                 }
                        else peerDetails
                                 { datagramHandle =
                                       Connected {connId = connHandle}
                                 }
            newPeerTvar <- newTVar newPeerDetails
            let newHashMap = HM.insert node newPeerTvar nodeIdPeerMap
            writeTVar nodeIdPeerMapTVar newHashMap)

{-Dummy Functions========================================================-}
-- selfNodeId :: NodeId
-- selfNodeId = pack "12334556"
getNewConnection :: IO (NodeId, ConnectionId, TransportType)
getNewConnection = return ("DSGNO", pack "892sadasd346384", UDP)

openConnection :: NodeId -> IP -> Port -> TransportType -> IO ConnectionId
openConnection nodeId ip port transportType = return (pack "892sadasd346384")

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()

readMessage :: ConnectionId -> IO ByteString
readMessage connId =
    Lazy.toStrict .
    serialise . generateP2PMessage Kademlia (pack "892sadasd346384") <$>
    getUUID
