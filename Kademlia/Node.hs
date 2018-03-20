{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

module Kademlia.Node
  ( runUDPServerForever,
    messageHandler,
    loadDefaultPeers,
    networkClient,
    addToKbChan,
    maintainPendingResChan
    -- refreshKbucket
  ) where

import           Control.Concurrent        (forkIO, newChan, newEmptyMVar,
                                            putMVar, readChan, takeMVar,
                                            threadDelay, writeChan,isEmptyChan,Chan,ThreadId)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
import qualified Network.Socket.ByteString as N (recv, recvFrom, sendAll,
                                                 sendAllTo, sendTo)
import           Control.Concurrent.Async
import           System.Environment
import           System.Random             (randomRIO)
import qualified Data.Map.Strict           as Map 
import           Data.Maybe 
import           System.Random
import           Data.Word 
import           Kademlia.Utils              
import qualified Kademlia.Types as T 

import           Codec.Serialise
import           Codec.Serialise.Encoding
import           Codec.Serialise.Decoding
import           Codec.Serialise.Class
import           Data.Time.Clock.POSIX 
import           Data.Time.Clock   
import           Crypto.PubKey.Ed25519
import qualified Data.ByteString.Lazy as LBS 

import Crypto.Util 
import GHC.Integer.Logarithms
import GHC.Exts
import Data.ByteString.Base16 as H 
import Data.ByteArray 
import Control.Concurrent.STM.TChan
import Control.Monad.STM 
import Control.Monad 

import Crypto.Utils.Keys.Signature 
import qualified Kademlia.Query as Q                
import Data.List as L 
import Control.Monad.Logger
import Control.Monad.IO.Class
import qualified Data.Text as DT 
import Kademlia.XorDistance 

-- Process all the incoming messages to server and write the response to outboundChan 
-- whenever a findNode message is recieved it write that peer to peerChan  
messageHandler :: T.NodeId  
               -> SecretKey
               -> TChan (SockAddr,Socket)
               -> TChan (T.PayLoad,SockAddr,SockAddr,Socket) 
               -> TChan (T.PayLoad,SockAddr,Socket) 
               -> TChan ((T.NodeId,T.NodeEndPoint),Int) 
               -> TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
               -> TChan (Map.Map C.ByteString [(T.Sequence,T.POSIXTime)])
               -> Chan (Loc, LogSource, LogLevel, LogStr)
               -> Int            
               -> Int                           
               -> IO ThreadId                   

messageHandler nodeId sk servChan inboundChan outboundChan peerChan kbChan pendingResChan logChan  k workerId = forkIO $ forever $ runChanLoggingT logChan $ do
    logInfoN (DT.pack ("Reading inboundChan, WorkderID : " ++ (show workerId)))
    msg <- liftIO $ atomically $ readTChan inboundChan

    let incMsg          = extractFirst2 msg
        socka           = extractThird2 msg 
        senderNodeId    = T.nodeId (T.messageBody(T.message (incMsg)))
        senderEndPoint  = T.fromEndPoint (T.messageBody(T.message (incMsg)))
        senderPublicKey = senderNodeId :: PublicKey 
        localSock       = extractThird2 msg 
        remoteSock      = extractSecond2 msg  
        localSocket     = extractFourth msg 
        msgSeq          = T.sequence (T.message incMsg)

        -- | Calculates distance and corresponding k-bucket index 
        dis             = ((Data.ByteArray.xor senderNodeId nodeId) :: C.ByteString)
        kbi             = I# (integerLog2# (bs2i dis))

    logInfoN (DT.pack ("WorkerID :  " ++ (show workerId) ++ " | Incoming Payload : " ++ (show incMsg)))

    case (T.messageType (T.message (incMsg)))  of 
        
        -- | handles the case when message type is MSG01 i.e PING 
        (T.MSG01) -> do   
            let payl = T.packPong nodeId sk socka (1)     
            liftIO $ atomically $ writeTChan outboundChan (payl,extractSecond2 msg,extractFourth msg)
       
        -- | handles the case when message type is MSG02 i.e PONG
        (T.MSG02) -> do 
            let payl = T.packPing nodeId sk socka (1) 
            liftIO $ atomically $ writeTChan outboundChan (payl,extractSecond2 msg,extractFourth msg)

        -- | handles the case when message type is MSG03 i.e FIND_NODE
        -- | Adds peer issuing FIND_NODE to it's appropriate k-bucket 
        (T.MSG03) -> do
            liftIO $ atomically $ writeTChan peerChan ((senderNodeId,senderEndPoint),kbi)

            -- | Queries k-buckets and send k-closest buckets 
            liftIO $ threadDelay 1000
            liftIO $ Q.queryKBucket nodeId senderNodeId k kbChan outboundChan localSock remoteSock localSocket sk msgSeq
        
        -- | handles the case when message type is MSG04 i.e FN_RESP
        (T.MSG04) -> do   
            let plist   = T.peerList (T.messageBody(T.message (incMsg)))
                kbil    = map (extractDistance nodeId) plist       
            isValid <- liftIO $ checkResponseValidity pendingResChan (senderNodeId,msgSeq)
            case (isValid) of 
                True      -> do 
                    liftIO $ atomically $ mapM_ (writeTChan peerChan) (kbil)
                    case (isPlistFilled plist) of 
                        True -> do 
                            temp <- liftIO $ replicateM (Prelude.length plist) (T.getRandomSequence) 
                            let payl         = Prelude.map (T.packFindMsg nodeId sk localSock nodeId) temp 
                                repl         = (Prelude.replicate (Prelude.length plist))
                                sockAddrList = Prelude.map (\x  -> getSockAddr (T.nodeIp x) (T.udpPort x)) $ (Prelude.map (snd) plist) 
                                tempm        = (zip payl (Prelude.map (fst) plist ))
                        
                            liftIO $ mapM_ (addToPendingResChan pendingResChan) tempm
                            liftIO $ atomically $ mapM_ (writeTChan outboundChan) (zip3 payl sockAddrList (repl localSocket)) 

                        otherwise -> do 
                            liftIO $ putStrLn "Cannot Send FIND_NODE becasue of empty FN_RESP"
                            liftIO $ putStrLn ""
                            logInfoN (DT.pack "Cannot Send FIND_NODE becasue of empty FN_RESP")
                
                otherwise -> do 
                    liftIO $ print "Invalid/timed out message OR empty pendingResChan"
                    liftIO $ putStrLn ""
                    logInfoN (DT.pack "Invalid/timed out message OR empty pendingResChan")

isPlistFilled plist 
    | (Prelude.length plist == 0) = False
    | otherwise                   = True 
                
-- | Sends the message written by outboundChan to remote Client 
-- | Responsible for reading outboundChan and sending the contents to mentioned address
networkClient :: TChan (T.PayLoad,SockAddr,Socket) 
              -> Chan (Loc, LogSource, LogLevel, LogStr) 
              -> Int 
              -> IO ThreadId

networkClient outboundChan logChan workerId = forkIO $ forever $ runChanLoggingT logChan $ do 
    msg <- liftIO $ atomically $ readTChan outboundChan
    let pl = serialise (extractFirst msg) 
    liftIO $ N.sendTo (extractThird msg) (LBS.toStrict pl) (extractSecond msg)
    logInfoN (DT.pack ( "WorkerId : " ++ (show workerId) ++ " | Outgoing Payload " ++ (show msg)))

      
-- | Runs on a seperate thread & and is responsible for writing to kbChan   
addToKbChan :: TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)]) 
            -> TChan ((T.NodeId,T.NodeEndPoint),Int) 
            -> Chan (Loc, LogSource, LogLevel, LogStr)
            -> Int 
            -> IO ThreadId 

addToKbChan kbChan peerChan logChan workerId = forkIO $ forever $ runChanLoggingT logChan $ do
    msg <- liftIO $ atomically $ readTChan peerChan 
    rl <- liftIO $ atomically $ isEmptyTChan kbChan 
    let temp4 = fst msg      
    
    case rl of 
        True -> do 
            let temp  = Map.empty 
                temp2 = Map.insert (snd msg) (temp4 : []) temp  
            liftIO $ atomically $ writeTChan kbChan temp2
            logInfoN (DT.pack ("Kbucket : " ++ show temp2))
            
        False -> do 
                kb <- liftIO $ atomically $ readTChan kbChan
                if (Map.lookup (snd msg) kb == Nothing)
                    then do
                        let temp = Map.insert (snd msg) (temp4:[]) kb 
                        liftIO $ atomically $ writeTChan kbChan temp 
                        logInfoN (DT.pack ("Kbucket : " ++ show temp))
                        
                    else do 
                        let temp    = Map.lookup (snd msg) kb 
                            temp2   = fromMaybe [] temp 
                        -- | Checks if the nodeId already exists in the HashMap 
                        if (isNodeIdElem temp2 (fst temp4) == False)
                            then do 
                                let temp3   = temp2 ++ (temp4 : [])
                                    payLoad = Map.insert (snd msg) (temp3) kb   
                                liftIO $ atomically $ writeTChan kbChan payLoad
                                logInfoN (DT.pack ("Kbucket : " ++ show payLoad))
                              
                            else do 
                                let payLoad = Map.insert (snd msg) (temp2) kb   
                                liftIO $ atomically $ writeTChan kbChan payLoad
                                logInfoN (DT.pack ("Kbucket : " ++ show payLoad))
                                                                             
-- | UDP server which is constantly listenting for requests
runUDPServerForever :: String 
                    -> String 
                    -> TChan(T.PayLoad,SockAddr,SockAddr,Socket) 
                    -> TChan (SockAddr,Socket) 
                    -> IO ()

runUDPServerForever local_ip local_port inboundChan servChan = do
    addrinfos <- getAddrInfo Nothing (Just local_ip) (Just local_port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    atomically $ writeTChan servChan ((addrAddress serveraddr),sock) 

    print ("Server now listening for requests at : " ++ local_port)
    putStrLn ""
    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            -- | Verifies the message and makes sure that the nodeID mentioned in the message signed it 
            let remoteMsg = (deserialise (LBS.fromStrict $ mesg) :: T.PayLoad)
                msgtemp  = T.message remoteMsg
                msgv     = (LBS.toStrict (serialise(msgtemp)))
                signtemp = T.signature remoteMsg :: Signature 
                pk       = T.nodeId $ T.messageBody (msgtemp) :: T.NodeId 
                ftemp    = verify (pk) msgv signtemp 
            case ftemp of 
                True -> atomically $ writeTChan inboundChan (remoteMsg,socaddr2,(addrAddress serveraddr),sock)
            
-- | Load Default peers into kbChan i.e K-buckets 
loadDefaultPeers :: T.NodeId 
                 -> SecretKey
                 -> [(T.NodeId,SockAddr)]
                 -> TChan (T.PayLoad,SockAddr,Socket) 
                 -> TChan ((T.NodeId,T.NodeEndPoint),Int) 
                 -> TChan (SockAddr,Socket) 
                 -> TChan (Map.Map C.ByteString [(T.Sequence,T.POSIXTime)])
                 -> IO ()

loadDefaultPeers nodeId sk peerList outboundChan peerChan servChan pendingResChan = do 
    msg    <- atomically $ readTChan servChan 
    temp   <- replicateM (Prelude.length peerList) (T.getRandomSequence) 
    
    let repl       = Prelude.replicate (Prelude.length peerList)
        socka      = fst msg
        peerList2  = Prelude.map (snd) peerList 
        nodeIdList = Prelude.map (fst) peerList 
        
        -- | Message containing FIND_NODE : Self 
        payl  = Prelude.map (T.packFindMsg nodeId sk socka nodeId) temp  

    -- | First messages are sent out and then tracked by adding them to pendingrespChan 
    mapM_ (addToPendingResChan pendingResChan) (zip payl nodeIdList)
    atomically $ mapM_ (writeTChan outboundChan) (zip3 payl peerList2 (repl (snd msg)))
    
addToPendingResChan :: TChan (Map.Map C.ByteString [(T.Sequence,T.POSIXTime)]) 
                    -> (T.PayLoad,T.NodeId)
                    -> IO ThreadId 

addToPendingResChan pendingResChan peerInfo = forkIO $ do 
    
    let msg         = peerInfo
        recvdPayl   = fst msg
        msgType     = T.messageType (T.message (recvdPayl))
        msgSeq      = T.sequence (T.message (recvdPayl))
        recvrNodeId = publicKeytoHex (snd msg)
    
    rl <- atomically $ isEmptyTChan pendingResChan 
    ts <- T.getPOSIXTime
    
    case rl of 
        True -> do 
            let temp  = Map.empty 
                temp2 = Map.insert recvrNodeId ((msgSeq,ts):[]) temp 
            atomically $ writeTChan pendingResChan temp2 

        False -> do 
            msg2 <- atomically $ readTChan pendingResChan  
            case (Map.lookup (recvrNodeId) msg2) of 
                Nothing   -> do 
                    let temp3 = Map.insert recvrNodeId ((msgSeq,ts):[]) msg2 
                    atomically $ writeTChan pendingResChan temp3 

                otherwise -> do 
                    let temp4 = Map.lookup (recvrNodeId) msg2 
                        temp5 = fromMaybe [] temp4
                        temp6 = (msgSeq,ts) 
                        temp7 = Map.insert recvrNodeId (temp5 ++ (temp6:[])) msg2 
                    atomically $ writeTChan pendingResChan temp7 

checkResponseValidity :: TChan (Map.Map C.ByteString [(T.Sequence,T.POSIXTime)]) 
                      -> (T.NodeId,T.Sequence)
                      -> IO Bool 
     
checkResponseValidity pendingResChan peerInfo = do 
    
    msg <- atomically $ readTChan pendingResChan 
    let peerNodeId  = publicKeytoHex (fst peerInfo)
        msgSeq      = snd peerInfo 
        seqList     = Map.lookup peerNodeId msg 
        peerSeqList = fromMaybe [] seqList
    
    ts <- T.getPOSIXTime
    case (elem msgSeq $ Prelude.map fst peerSeqList) of 
                
        True  -> do
            let newList = L.deleteBy (\(x,y) (a,b) -> a==x) (msgSeq,ts) peerSeqList
            case (Prelude.length newList) of 
                (0)         -> do 
                    let newMap = Map.delete peerNodeId msg 
                    atomically $ writeTChan pendingResChan newMap
                    return True
                            
                otherwise   -> do 
                    let newMap  = Map.insert peerNodeId newList msg 
                    atomically $ writeTChan pendingResChan newMap  
                    return True 
            
        False -> do 
            return (False)  

isExpired :: T.POSIXTime -> T.POSIXTime -> T.POSIXTime -> Bool                
isExpired ts1 ts2 rt
    | (Prelude.abs (ts1 - ts2) > rt) = True 
    | otherwise                      = False 

removeIfExpired pendingResChan rt map key = do
    ts  <- T.getPOSIXTime  
    let peerInfoTupleList = fromMaybe [] $ Map.lookup key map 
        temp = [x | x <- peerInfoTupleList , not $ isExpired ts (snd x) rt]
        temp2 = Map.insert key temp map 
    atomically $ writeTChan pendingResChan temp2  

maintainPendingResChan :: TChan (Map.Map C.ByteString [(T.Sequence,T.POSIXTime)]) 
                       -> T.POSIXTime
                       -> Int   
                       -> Int 
                       -> IO ThreadId 
                       
maintainPendingResChan pendingResChan responseTime threadDel workerId = forkIO $ forever $ do 
    msg <- atomically $ readTChan pendingResChan
    let keys = Map.keys msg
    mapM_ (removeIfExpired pendingResChan responseTime msg) keys 
    threadDelay threadDel 

extractDistance :: T.NodeId 
                -> (T.NodeId,T.NodeEndPoint) 
                -> ((T.NodeId,T.NodeEndPoint),Int) 

extractDistance localNodeId x  = (((fst x),(snd x)),kbi)   
    where temp = (fst x)     :: PublicKey 
          nid  = localNodeId :: PublicKey 
          dis  = Data.ByteArray.xor temp nid :: C.ByteString 
          kbi  = I# (integerLog2# (bs2i dis)) 