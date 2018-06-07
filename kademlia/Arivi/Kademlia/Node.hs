{-# LANGUAGE MagicHash #-}

module Arivi.Kademlia.Node
  (
    messageHandler,
    loadDefaultPeers,
    addToKbChan,
    maintainPendingResChan
  ) where

import           Control.Concurrent                (Chan, MVar, ThreadId,
                                                    forkIO, readMVar,
                                                    threadDelay)

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Env
import qualified Arivi.Kademlia.Query              as Q
import qualified Arivi.Kademlia.Types              as T
import           Arivi.Kademlia.Utils
import           Arivi.Network.Connection          (ipAddress, port)
import           Arivi.Network.Instance
import           Arivi.Network.Types               as ANT
import qualified Control.Concurrent.Lifted         as CCL (fork)
import           Control.Concurrent.STM.TChan      (TChan, isEmptyTChan,
                                                    readTChan, writeTChan)
import           Control.Monad                     (forever, mapM_, replicateM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.STM                 (atomically)
import           Crypto.Util
import           Data.ByteArray
import qualified Data.ByteString.Char8             as C (ByteString)
import           Data.List                         as L
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Text                         as DT
import qualified Data.Time.Clock.POSIX             as Clock (POSIXTime,
                                                             getPOSIXTime)
import           GHC.Exts
import           GHC.Integer.Logarithms
-- | Process all the incoming messages to server and write the response to
--   outboundChan whenever a findNode message is recieved it write that peer to
--   peerChan
-- TODO implement the logger functionality
messageHandler :: HasAriviNetworkInstance m => T.NodeId
               -> SecretKey
               -> ANT.ConnectionId
               -> T.PayLoad
               -> TChan ((T.NodeId,T.NodeEndPoint),Int)
               -> TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
               -> TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
               -> Chan (Loc, LogSource, LogLevel, LogStr)
               -> Int
               -> Int
               -> m ThreadId

messageHandler nodeId sk ariviConnectionId msg peerChan kbChan pendingResChan logChan
    k workerId = CCL.fork $ forever $ do

        ts <- liftIO Clock.getPOSIXTime

        -- TODO replace `1` below with a valid sequence
        let localSock = undefined

        ariviConnection <- lookupCId ariviConnectionId

        let remotePort  = port ariviConnection
            remoteIp    = stringToHostAddress $ ipAddress ariviConnection
            rs          = convToSockAddr remotePort remoteIp

        let incMsg          = msg
            remoteSock      = rs
            senderNodeId    = T.nodeId (T.messageBody(T.message incMsg))
            senderEndPoint  = T.fromEndPoint (T.messageBody(T.message incMsg))
            senderPublicKey = senderNodeId :: PublicKey
            msgSeq          = T.sequence (T.message incMsg)

            dis             =  Data.ByteArray.xor senderNodeId nodeId
                                :: C.ByteString
            kbi             = I# (integerLog2# (bs2i dis))

        case T.messageType (T.message incMsg)  of

            -- handles the case when message type is MSG01 i.e PING
            T.MSG01 -> do
                let payl = T.packPong nodeId sk localSock 1
                sendMessage ariviConnectionId $ serialise payl
                -- liftIO $ print ""

            -- handles the case when message type is MSG02 i.e PONG
            T.MSG02 -> do
                let payl = T.packPing nodeId sk localSock 1
                sendMessage ariviConnectionId $ serialise payl
                -- liftIO $ print ""

            -- handles the case when message type is MSG03 i.e FIND_NODE
            -- Adds peer issuing FIND_NODE to it's appropriate k-bucket
            T.MSG03 -> do
                liftIO $ atomically $ writeTChan peerChan
                    ((senderNodeId,senderEndPoint),kbi)

                -- Queries k-buckets and send k-closest buckets
                liftIO $ threadDelay 1000
                Q.queryKBucket nodeId senderNodeId k kbChan
                    ariviConnectionId sk 1

            -- handles the case when message type is MSG04 i.e FN_RESP
            T.MSG04 -> do
                let plist   = T.peerList (T.messageBody(T.message incMsg))
                    kbil    = map (extractDistance nodeId) plist
                (isValidRequest,tstemp) <- liftIO $ checkResponseValidity
                    pendingResChan (senderNodeId,msgSeq)
                if isValidRequest then
                    do
                        liftIO $ atomically $ mapM_ (writeTChan peerChan) kbil
                        if isPlistFilled plist then
                                if isExpired ts tstemp 10 then
                                    do
                                        temp <- liftIO $
                                            replicateM (Prelude.length plist)
                                            getRandomSequence
                                        let payl         = Prelude.map
                                                            (T.packFindMsg
                                                            nodeId sk localSock
                                                             nodeId)
                                                             temp
                                            repl         =  Prelude.replicate
                                                            (Prelude.length
                                                            plist)
                                            sockAddrList = Prelude.map (\ x ->
                                                            getSockAddr
                                                            (T.nodeIp x)
                                                            (T.udpPort x))
                                            tempm        =  zip payl
                                                            (Prelude.map fst
                                                             plist )

                                        liftIO $ mapM_ (addToPendingResChan
                                                            pendingResChan)
                                                            tempm
                                        -- TODO import sendMesage from Arivi.Network
                                        -- liftIO $ mapM_ (Arivi.send)
                                        -- (zip payl sockAddrList)
                                        liftIO $ print ""
                                    else liftIO $ print ""

                        else do
                                    liftIO $ print "Cannot Send FIND_NODE becasueof empty FN_RESP"
                                    liftIO $ putStrLn ""

                else
                    do
                        liftIO $ print "Invalid/timed out message OR empty pendingResChan"
                        liftIO $ putStrLn ""

isPlistFilled :: Foldable t => t a -> Bool
isPlistFilled plist
    | L.null plist = False
    | otherwise    = True

-- | Runs on a seperate thread & and is responsible for writing to kbChan
addToKbChan :: TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
            -> TChan ((T.NodeId,T.NodeEndPoint),Int)
            -> Chan (Loc, LogSource, LogLevel, LogStr)
            -> Int
            -> IO ThreadId

addToKbChan kbChan peerChan logChan workerId = forkIO $ forever $
    runChanLoggingT logChan $ do
    msg <- liftIO $ atomically $ readTChan peerChan
    rl <- liftIO $ atomically $ isEmptyTChan kbChan
    let temp4 = fst msg

    if rl
    then do
            let temp  = Map.empty
                temp2 = Map.insert (snd msg) [temp4] temp
            liftIO $ atomically $ writeTChan kbChan temp2
            logInfoN (DT.pack ("Kbucket : " ++ show temp2))

    else do
        kb <- liftIO $ atomically $ readTChan kbChan
        if isNothing (Map.lookup (snd msg) kb)
        then do
            let temp = Map.insert (snd msg) [temp4] kb
            liftIO $ atomically $ writeTChan kbChan temp
            logInfoN (DT.pack ("Kbucket : " ++ show temp))

        else do
            let temp    = Map.lookup (snd msg) kb
                temp2   = fromMaybe [] temp
                -- Checks if the nodeId already exists in the HashMap
            if not (isNodeIdElem temp2 (fst temp4))
            then do
                let temp3   = temp2 ++ [temp4]
                    payLoad = Map.insert (snd msg) temp3 kb
                liftIO $ atomically $ writeTChan kbChan payLoad
                logInfoN (DT.pack ("Kbucket 1 : " ++ show payLoad))

            else do
                let payLoad = Map.insert (snd msg) temp2 kb
                liftIO $ atomically $ writeTChan kbChan payLoad
                logInfoN (DT.pack ("Kbucket 2 : " ++ show payLoad))


-- | Load Default peers into kbChan i.e K-buckets
loadDefaultPeers :: T.NodeId
                 -> SecretKey
                 -> [(T.NodeId,SockAddr)]
                 -> TChan ((T.NodeId,T.NodeEndPoint),Int)
                 -> MVar SockAddr
                 -> TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
                 -> IO ThreadId

loadDefaultPeers nodeId sk peerList peerChan localSock pendingResChan =
    forkIO $ do

    temp   <- replicateM (Prelude.length peerList) getRandomSequence
    socka  <- readMVar localSock

    let repl       = Prelude.replicate (Prelude.length peerList)
        peerList2  = Prelude.map snd peerList
        nodeIdList = Prelude.map fst peerList

        -- Message containing FIND_NODE : Self
        payl  = Prelude.map (T.packFindMsg nodeId sk socka nodeId) temp

    -- First messages are sent out and then tracked by adding them to
    --   pendingrespChan
    mapM_ (addToPendingResChan pendingResChan) (zip payl nodeIdList)
    -- mapM_ (Arivi.send) (zip payl peerList2)
    print ""

addToPendingResChan :: TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
                    -> (T.PayLoad,T.NodeId)
                    -> IO ThreadId

addToPendingResChan pendingResChan peerInfo = forkIO $ do

    let msg         = peerInfo
        recvdPayl   = fst msg
        msgType     = T.messageType (T.message recvdPayl)
        msgSeq      = T.sequence (T.message recvdPayl)
        recvrNodeId = publicKeytoHex (snd msg)

    rl <- atomically $ isEmptyTChan pendingResChan
    ts <- Clock.getPOSIXTime

    if rl then
        do
            let temp  = Map.empty
                temp2 = Map.insert recvrNodeId [(msgSeq,ts)] temp
            atomically $ writeTChan pendingResChan temp2

    else
        do
            msg2 <- atomically $ readTChan pendingResChan
            case Map.lookup recvrNodeId msg2 of
                Nothing -> do
                    let temp3 = Map.insert recvrNodeId [(msgSeq,ts)] msg2
                    atomically $ writeTChan pendingResChan temp3

                _       -> do
                    let temp4 = Map.lookup recvrNodeId msg2
                        temp5 = fromMaybe [] temp4
                        temp6 = (msgSeq,ts)
                        temp7 = Map.insert recvrNodeId (temp5 ++ [temp6]) msg2
                    atomically $ writeTChan pendingResChan temp7

checkResponseValidity :: TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
                      -> (T.NodeId,T.Sequence)
                      -> IO (Bool,Clock.POSIXTime)

checkResponseValidity pendingResChan peerInfo = do
    msg <- atomically $ readTChan pendingResChan
    let peerNodeId  = publicKeytoHex (fst peerInfo)
        msgSeq      = snd peerInfo
        seqList     = Map.lookup peerNodeId msg
        peerSeqList = fromMaybe [] seqList
        tempf       = elem msgSeq $ Prelude.map fst peerSeqList

    case tempf of
        True    -> return (True,tstemp)
                   where tstemp = fromMaybe 0 (L.lookup msgSeq peerSeqList)
        _       -> return (False,0::Clock.POSIXTime)

isExpired :: Clock.POSIXTime -> Clock.POSIXTime -> Clock.POSIXTime -> Bool
isExpired ts1 ts2 rt
    | Prelude.abs (ts1 - ts2) > rt = True
    | otherwise                      = False

removeIfExpired :: Ord t => TChan (Map.Map t [(a, Clock.POSIXTime)]) -> Clock.POSIXTime -> Map.Map t [(a, Clock.POSIXTime)] -> t -> IO ()
removeIfExpired pendingResChan rt map key = do
    ts  <- Clock.getPOSIXTime
    let peerInfoTupleList = fromMaybe [] $ Map.lookup key map
        temp = [x | x <- peerInfoTupleList , not $ isExpired ts (snd x) rt]
        temp2 = Map.insert key temp map
    atomically $ writeTChan pendingResChan temp2

maintainPendingResChan :: TChan (Map.Map C.ByteString
                            [(T.Sequence,Clock.POSIXTime)])
                       -> Clock.POSIXTime
                       -> Int
                       -> Int
                       -> IO ThreadId

maintainPendingResChan pendingResChan responseTime threadDel workerId =
    forkIO $ forever $ do

    msg <- atomically $ readTChan pendingResChan
    let keys = Map.keys msg
    mapM_ (removeIfExpired pendingResChan responseTime msg) keys
    threadDelay threadDel

extractDistance :: T.NodeId
                -> (T.NodeId,T.NodeEndPoint)
                -> ((T.NodeId,T.NodeEndPoint),Int)

extractDistance localNodeId x  = (x,kbi)
    where temp = fst x       :: PublicKey
          nid  = localNodeId :: PublicKey
          dis  = Data.ByteArray.xor temp nid :: C.ByteString
          kbi  = I# (integerLog2# (bs2i dis))

