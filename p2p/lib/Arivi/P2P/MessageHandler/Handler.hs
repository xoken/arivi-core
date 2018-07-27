{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Arivi.P2P.MessageHandler.Handler
    ( sendRequest
    , sendRequestforKademlia
    , newIncomingConnection
    , cleanConnection
    ) where

import           Arivi.Network                         (openConnection)

import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.Utils.Logging
import           Codec.Serialise                       (deserialise, serialise)
import qualified Control.Concurrent.Async              as Async (race)
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)
import           Control.Concurrent.Lifted             (threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue         ()
import           Control.Concurrent.STM.TVar           ()
import           Control.Exception                     (displayException, throw)
import qualified Control.Exception.Lifted              as Exception (SomeException,
                                                                     try)
import           Control.Monad                         (when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (logDebug)
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8            as BSLC (pack)
import           Data.HashMap.Strict                   as HM
import           Data.Maybe
import qualified Data.UUID                             as UUID (toString)
import           Data.UUID.V4                          (nextRandom)
import           Network.Socket                        (PortNumber)

import           Data.String.Conv
import qualified Data.Text                             as T

-- | used by RPC and PubSub to send outgoing requests. This is a blocing call which returns the reply
sendRequest ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> P2PPayload
    -> m P2PPayload
sendRequest node mType p2pPayload = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    newuuid <- liftIO getUUID
    mvar <- liftIO newEmptyMVar
    (connHandle, newFlag) <- getConnHandleFromNodeID node nodeIdMapTVar mType
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    let peerDetailsTVar = fromJust (HM.lookup node nodeIdMap)
    peerDetails <- liftIO $ readTVarIO peerDetailsTVar
    let uuidMapTVar = tvarUUIDMap peerDetails
    when newFlag $ do
        messageTypeMap <- getMessageTypeMapP2PEnv
        _ <-
            LAsync.async
                (readRequestThread connHandle uuidMapTVar messageTypeMap)
        return ()
    liftIO $
        atomically
            (do uuidMap <- readTVar uuidMapTVar
                let newUUIDMAP = HM.insert newuuid mvar uuidMap
                writeTVar uuidMapTVar newUUIDMAP)
    let p2pMessage = generateP2PMessage mType p2pPayload newuuid
    res <- Exception.try $ send connHandle (serialise p2pMessage)
    case res of
        Left (e :: Exception.SomeException) -> do
            liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
            throw e
        Right _ -> do
            $(logDebug) "Request sent"
            winner <-
                liftIO $
                Async.race
                    (threadDelay 30000000) -- system.timeout
                    (takeMVar mvar :: IO P2PMessage)
            case winner of
                Left _ -> do
                    liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
                    $(logDebug) "TimerExpiredMVar"
                    return $ BSLC.pack "Timer expired"
                Right (p2pReturnMessage :: P2PMessage) -> do
                    $(logDebug) "got response"
                    $(logDebug) (toS $ show p2pReturnMessage)
                    liftIO $ atomically (deleteUUID newuuid uuidMapTVar)
                    let returnMessage = payload p2pReturnMessage
                    return returnMessage

sendRequestforKademlia ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> MessageType
    -> P2PPayload
    -> PortNumber
    -> IP
    -> m P2PPayload
sendRequestforKademlia node mType p2pPayload port mIP
    -- \$(logDebug) (toS $ show p2pPayload)
 = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    let maybePeer = HM.lookup node nodeIdMap
    if isNothing maybePeer -- concurrency issues might arise here need to check
        then do
            res <- openConnection mIP port UDP node
            case res of
                Left e -> do
                    $(logDebug) (T.pack (displayException e))
                    return $ BSLC.pack (displayException e ++ show mIP)
                Right connHandle -> do
                    $(logDebug) (toS $ show node)
                    $(logDebug) (toS $ show mType)
                    $(logDebug) (toS $ show port)
                    $(logDebug) (toS $ show mIP)
                    $(logDebug) "Connectionhandle made"
                    liftIO $
                        addPeerFromConnection node UDP connHandle nodeIdMapTVar
                    newNodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
                    let peer = fromJust (HM.lookup node newNodeIdMap)
                    peerDetails <- liftIO $ readTVarIO peer
                    let uuidMapTVar = tvarUUIDMap peerDetails
                    messageTypeMap <- getMessageTypeMapP2PEnv
                    _ <-
                        LAsync.async $
                        readRequestThread connHandle uuidMapTVar messageTypeMap
                    sendRequest node mType p2pPayload
        else sendRequest node mType p2pPayload

readRequestThread ::
       (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar UUIDMap
    -> MessageTypeMap m
    -> m ()
readRequestThread connHandle uuidMapTVar messageTypeMap =
    $(withLoggingTH) (LogP2PStatement "readRequestThread: ") LevelDebug $ do
        eitherByteMessage <- Exception.try $ recv connHandle
        case eitherByteMessage of
            -- match some concrete network exception and do something
            Left (e :: Exception.SomeException) -> do
                $(logDebug) (T.pack (displayException e))
                return ()
            Right byteMessage -> do
                _ <-
                    do $(logDebug) "Recieved incoming message"
                       LAsync.async
                           (processIncomingMessage
                                connHandle
                                uuidMapTVar
                                messageTypeMap
                                byteMessage)
                readRequestThread connHandle uuidMapTVar messageTypeMap

-- newConnectionHandler :: NodeId -> ConnectionHandle -> TransportType ->
newIncomingConnection ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> TransportType
    -> ConnectionHandle
    -> m ()
newIncomingConnection mNodeId transportType connHandle = do
    $(logDebug) "Got new incoming connection handle"
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    messageTypeMap <- getMessageTypeMapP2PEnv
    liftIO $
        addPeerFromConnection mNodeId transportType connHandle nodeIdMapTVar
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    peerDetails <- liftIO $ readTVarIO (fromJust (HM.lookup mNodeId nodeIdMap))
    let uuidMapTVar = tvarUUIDMap peerDetails
    _ <- LAsync.async (readRequestThread connHandle uuidMapTVar messageTypeMap)
    return ()


-- Please be in STM..Pretty please :(
cleanConnection ::
       (HasP2PEnv m, HasLogging m) => NodeId -> TransportType -> m ()
cleanConnection mNodeId transportType = do
    $(logDebug) "Cleaned ConnectionHandle"
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    -- aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaahhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
    let peerDetailsTVar = fromJust (HM.lookup mNodeId nodeIdMap)
    -- please use modifyTVar
    liftIO $
        atomically
            (do peerDetails <- readTVar peerDetailsTVar
                let newPeerDetails =
                        case transportType of
                            UDP ->
                                case datagramHandle peerDetails of
                                    Connected _ ->
                                        peerDetails
                                            {datagramHandle = NotConnected}
                                    _ -> peerDetails
                            TCP ->
                                case streamHandle peerDetails of
                                    Connected _ ->
                                        peerDetails
                                            {streamHandle = NotConnected}
                                    _ -> peerDetails
                writeTVar peerDetailsTVar newPeerDetails)
    liftIO $ cleanPeer mNodeId nodeIdMapTVar
    return ()
 --TODO: need to do uuidcheck in this thread and spawn only on a request

processIncomingMessage ::
       forall m. (HasP2PEnv m, HasLogging m)
    => ConnectionHandle
    -> TVar UUIDMap
    -> MessageTypeMap m
    -> Lazy.ByteString
    -> m ()
processIncomingMessage connHandle uuidMapTVar messageTypeMap byteMessage =
    $(withLoggingTH) (LogP2PStatement "processIncomingMessage: ") LevelInfo $ do
        -- why check for deserialisation failure?! ¯\_(ツ)_/¯
        let networkMessage = deserialise byteMessage :: P2PMessage
        uuidMap <- liftIO $ atomically (readTVar uuidMapTVar)
        let temp = HM.lookup (uuid networkMessage) uuidMap
        -- use case
        if isNothing temp
            then do
                $(logDebug) "incoming request recieved"
                let fnc =
                        fromJust $
                        HM.lookup (messageType networkMessage) messageTypeMap
                -- response <- Exception.try $ fnc (payload networkMessage) :: m (Either AriviP2PException P2PPayload)
                response <-
                    Exception.try (fnc (payload networkMessage)) :: m (Either AriviP2PException P2PPayload)
                case response of
                    -- what kind of generalized assumption is that the exception is a deserialiseFailure?! ¯\_(ツ)_/¯
                    Left e ->
                        $(logDebug) $
                        T.append
                            (T.pack
                                 "Couldn't deserialise message while handiling incoming msg : ")
                            (T.pack (displayException e))
                    Right response' -> do
                        let p2pResponse =
                                generateP2PMessage
                                    (messageType networkMessage)
                                    response'
                                    (uuid networkMessage)
                        -- Why call a function processIncomingMessage and also use it to send stuff over the network. Why not launch some missile here too ¯\_(ツ)_/¯
                        res <-
                            Exception.try $
                            send connHandle (serialise p2pResponse)
                        case res of
                            Left (e :: Exception.SomeException) --TODO: ExceptionHadnling >> Ek din aayega..tuu handle hoyega..aaaaaaaaaaaaaaaaaaaaaaaaaa
                             -> do
                                $(logDebug) (T.pack (displayException e))
                                return ()
                            Right _ -> return ()
            else do
                let mVar = fromJust temp
                liftIO $ putMVar mVar networkMessage
                return ()

{-Support Functions===========================================================-}
-- Which function are you talking about?! :'( | atomically checks for existing handle which is returned if it exists or else its status is changed to pending. then a new connection is established and it is stored as well as returned.
-- Why you no be in STM?! :'(
cleanPeer :: NodeId -> TVar NodeIdPeerMap -> IO ()
cleanPeer mNodeId nodeIdMapTVar =
    atomically
        (do nodeIdMap <- readTVar nodeIdMapTVar
            let maybePeer = HM.lookup mNodeId nodeIdMap
            case maybePeer of
                Just peerDetailsTVar -> do
                    peerDetails <- readTVar peerDetailsTVar
                    case peerDetails of
                        -- what kind of stupid case is this?!
                        PeerDetails {} -> do
                            let newnodeIdMap = HM.delete mNodeId nodeIdMap
                            writeTVar nodeIdMapTVar newnodeIdMap
                            return ()
                        _ -> return ()
                Nothing -> return ())

getConnectionHandle ::
       (HasP2PEnv m, HasLogging m)
    => TVar PeerDetails
    -> TransportType
    -> m (ConnectionHandle, Bool)
getConnectionHandle peerDetailsTVar transportType = do
    peerDetails <- liftIO $ readTVarIO peerDetailsTVar
    let connMaybe =
            if transportType == TCP
                then streamHandle peerDetails
                else datagramHandle peerDetails
    case connMaybe of
        NotConnected -> do
            mCheck <-
                liftIO $
                atomically
                    (changeConnectionStatus peerDetailsTVar transportType)
            if mCheck
                then do
                    res <-
                        openConnection
                            (fromJust (ip peerDetails))
                            (if transportType == TCP
                                 then fromJust (tcpPort peerDetails)
                                 else fromJust (udpPort peerDetails))
                            transportType
                            (nodeId peerDetails)
                    case res of
                        Left e -> do
                            $(logDebug) (T.pack (displayException e))
                            getConnectionHandle peerDetailsTVar transportType
                        Right connHandle -> do
                            liftIO $
                                atomically
                                    (do oldPeerDetails <-
                                            readTVar peerDetailsTVar
                                        let newPeerDetails =
                                                if transportType == TCP
                                                    then oldPeerDetails
                                                             { streamHandle =
                                                                   Connected connHandle
                                                             }
                                                    else oldPeerDetails
                                                             { datagramHandle =
                                                                   Connected connHandle
                                                             }
                                        writeTVar peerDetailsTVar newPeerDetails)
                            return (connHandle, True)
                else getConnectionHandle peerDetailsTVar transportType
        Pending -> do
            liftIO $ threadDelay 3000 --should depend on avg time to open connection
            getConnectionHandle peerDetailsTVar transportType
        Connected connHandle -> return (connHandle, False)

-- | if connhandle is NotConnected then change it to Pending. Should be done atomically
changeConnectionStatus :: TVar PeerDetails -> TransportType -> STM Bool
changeConnectionStatus peerDetailsTVar transportType = do
    -- Use modifyTVar
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
        -- Should pending and connected both return NotConnected
        else return False

-- | delete an uuid entry from the map
deleteUUID :: P2PUUID -> TVar UUIDMap -> STM ()
deleteUUID mUUID uuidMapTVar = do
    a <- readTVar uuidMapTVar
    let b = HM.delete mUUID a
    writeTVar uuidMapTVar b

-- | get connection handle for the specific nodeID and mesaage type from the hashmap
getConnHandleFromNodeID ::
       (HasP2PEnv m, HasLogging m)
    => NodeId
    -> TVar NodeIdPeerMap
    -> MessageType
    -> m (ConnectionHandle, Bool)
getConnHandleFromNodeID node nodeIdMapTVar mType = do
    nodeIdMap <- liftIO $ readTVarIO nodeIdMapTVar
    -- Again rampant uncheked use of fromJust
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
       NodeId
    -> TransportType
    -> ConnectionHandle
    -> TVar NodeIdPeerMap
    -> IO () -- Can be in STM
addPeerFromConnection node transportType connHandle nodeIdPeerMapTVar = do

    uuidMapTVar <- newTVarIO HM.empty
    atomically
    -- consider using modifyTVar
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
            -- Adds in the default case of maybe too. Will have a peer details record with everything as Nothing and either stream handle or datagram handle populated. openConnection with such a PeerDetails record would fail
            let newPeerDetails =
                    if transportType == TCP
                        then peerDetails
                                 { streamHandle =
                                       Connected connHandle
                                 }
                        else peerDetails
                                 { datagramHandle =
                                       Connected connHandle
                                 }
            newPeerTvar <- newTVar newPeerDetails
            let newHashMap = HM.insert node newPeerTvar nodeIdPeerMap
            writeTVar nodeIdPeerMapTVar newHashMap)
{-Dummy Functions========================================================-}
-- selfNodeId :: NodeId
-- selfNodeId = pack "12334556"
-- getNewConnection :: IO (NodeId, ConnectionHandle, TransportType)
-- getNewConnection = return (pack "DSGNO", pack "892sadasd346384", UDP)
-- openConnection :: NodeId -> IP -> PortNumber -> TransportType -> IO ConnectionHandle
-- openConnection nodeId ip port transportType = return (pack "892sadasd346384")
-- sendMessage :: ConnectionHandle -> Char8.ByteString -> IO ()
-- sendMessage connectionId byteString = return ()
-- readMessage :: ConnectionHandle -> IO ByteString
-- readMessage connId =
--     Lazy.toStrict .
--     serialise . generateP2PMessage Kademlia (pack "892sadasd346384") <$>
--     getUUID
