module Arivi.P2P.Kademlia.Node
    -- loadDefaultPeers,
    -- addToKbChan,
    -- maintainPendingResChan
    (
    ) where

import           Control.Concurrent                (Chan, MVar, ThreadId,
                                                    forkIO, readMVar,
                                                    threadDelay)

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Env
import           Arivi.Network.Connection          (ipAddress, port)
import           Arivi.Network.Instance
import           Arivi.Network.Types               as ANT
import qualified Arivi.P2P.Kademlia.Types          as T
import           Arivi.P2P.Kademlia.Utils
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
import           Network.Socket                    (SockAddr (..))
-- isPlistFilled :: Foldable t => t a -> Bool
-- isPlistFilled plist
--     | L.null plist = False
--     | otherwise    = True
-- -- | Runs on a seperate thread & and is responsible for writing to kbChan
-- addToKbChan :: TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
--             -> TChan ((T.NodeId,T.NodeEndPoint),Int)
--             -> Chan (Loc, LogSource, LogLevel, LogStr)
--             -> Int
--             -> IO ThreadId
-- addToKbChan kbChan peerChan logChan workerId = forkIO $ forever $
--     runChanLoggingT logChan $ do
--     msg <- liftIO $ atomically $ readTChan peerChan
--     rl <- liftIO $ atomically $ isEmptyTChan kbChan
--     let temp4 = fst msg
--     if rl
--     then do
--             let temp  = Map.empty
--                 temp2 = Map.insert (snd msg) [temp4] temp
--             liftIO $ atomically $ writeTChan kbChan temp2
--             logInfoN (DT.pack ("Kbucket : " ++ show temp2))
--     else do
--         kb <- liftIO $ atomically $ readTChan kbChan
--         if isNothing (Map.lookup (snd msg) kb)
--         then do
--             let temp = Map.insert (snd msg) [temp4] kb
--             liftIO $ atomically $ writeTChan kbChan temp
--             logInfoN (DT.pack ("Kbucket : " ++ show temp))
--         else do
--             let temp    = Map.lookup (snd msg) kb
--                 temp2   = fromMaybe [] temp
--                 -- Checks if the nodeId already exists in the HashMap
--             if not (isNodeIdElem temp2 (fst temp4))
--             then do
--                 let temp3   = temp2 ++ [temp4]
--                     payLoad = Map.insert (snd msg) temp3 kb
--                 liftIO $ atomically $ writeTChan kbChan payLoad
--                 logInfoN (DT.pack ("Kbucket 1 : " ++ show payLoad))
--             else do
--                 let payLoad = Map.insert (snd msg) temp2 kb
--                 liftIO $ atomically $ writeTChan kbChan payLoad
--                 logInfoN (DT.pack ("Kbucket 2 : " ++ show payLoad))
-- -- | Load Default peers into kbChan i.e K-buckets
-- loadDefaultPeers :: T.NodeId
--                  -> SecretKey
--                  -> [(T.NodeId,SockAddr)]
--                  -> TChan ((T.NodeId,T.NodeEndPoint),Int)
--                  -> MVar SockAddr
--                  -> TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
--                  -> IO ThreadId
-- loadDefaultPeers nodeId sk peerList peerChan localSock pendingResChan =
--     forkIO $ do
--     temp   <- replicateM (Prelude.length peerList) getRandomSequence
--     socka  <- readMVar localSock
--     let repl       = Prelude.replicate (Prelude.length peerList)
--         peerList2  = Prelude.map snd peerList
--         nodeIdList = Prelude.map fst peerList
--         -- Message containing FIND_NODE : Self
--         payl  = Prelude.map (T.packFindMsg nodeId sk socka nodeId) temp
--     -- First messages are sent out and then tracked by adding them to
--     --   pendingrespChan
--     mapM_ (addToPendingResChan pendingResChan) (zip payl nodeIdList)
--     -- mapM_ (Arivi.send) (zip payl peerList2)
--     print ""
-- addToPendingResChan :: TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
--                     -> (T.PayLoad,T.NodeId)
--                     -> IO ThreadId
-- addToPendingResChan pendingResChan peerInfo = forkIO $ do
--     let msg         = peerInfo
--         recvdPayl   = fst msg
--         msgType     = T.messageType (T.message recvdPayl)
--         msgSeq      = T.sequence (T.message recvdPayl)
--         recvrNodeId = publicKeytoHex (snd msg)
--     rl <- atomically $ isEmptyTChan pendingResChan
--     ts <- Clock.getPOSIXTime
--     if rl then
--         do
--             let temp  = Map.empty
--                 temp2 = Map.insert recvrNodeId [(msgSeq,ts)] temp
--             atomically $ writeTChan pendingResChan temp2
--     else
--         do
--             msg2 <- atomically $ readTChan pendingResChan
--             case Map.lookup recvrNodeId msg2 of
--                 Nothing -> do
--                     let temp3 = Map.insert recvrNodeId [(msgSeq,ts)] msg2
--                     atomically $ writeTChan pendingResChan temp3
--                 _       -> do
--                     let temp4 = Map.lookup recvrNodeId msg2
--                         temp5 = fromMaybe [] temp4
--                         temp6 = (msgSeq,ts)
--                         temp7 = Map.insert recvrNodeId (temp5 ++ [temp6]) msg2
--                     atomically $ writeTChan pendingResChan temp7
-- checkResponseValidity :: TChan (Map.Map C.ByteString [(T.Sequence,Clock.POSIXTime)])
--                       -> (T.NodeId,T.Sequence)
--                       -> IO (Bool,Clock.POSIXTime)
-- checkResponseValidity pendingResChan peerInfo = do
--     msg <- atomically $ readTChan pendingResChan
--     let peerNodeId  = publicKeytoHex (fst peerInfo)
--         msgSeq      = snd peerInfo
--         seqList     = Map.lookup peerNodeId msg
--         peerSeqList = fromMaybe [] seqList
--         tempf       = elem msgSeq $ Prelude.map fst peerSeqList
--     case tempf of
--         True    -> return (True,tstemp)
--                    where tstemp = fromMaybe 0 (L.lookup msgSeq peerSeqList)
--         _       -> return (False,0::Clock.POSIXTime)
-- isExpired :: Clock.POSIXTime -> Clock.POSIXTime -> Clock.POSIXTime -> Bool
-- isExpired ts1 ts2 rt
--     | Prelude.abs (ts1 - ts2) > rt = True
--     | otherwise                      = False
-- removeIfExpired :: Ord t => TChan (Map.Map t [(a, Clock.POSIXTime)]) -> Clock.POSIXTime -> Map.Map t [(a, Clock.POSIXTime)] -> t -> IO ()
-- removeIfExpired pendingResChan rt map key = do
--     ts  <- Clock.getPOSIXTime
--     let peerInfoTupleList = fromMaybe [] $ Map.lookup key map
--         temp = [x | x <- peerInfoTupleList , not $ isExpired ts (snd x) rt]
--         temp2 = Map.insert key temp map
--     atomically $ writeTChan pendingResChan temp2
-- maintainPendingResChan :: TChan (Map.Map C.ByteString
--                             [(T.Sequence,Clock.POSIXTime)])
--                        -> Clock.POSIXTime
--                        -> Int
--                        -> Int
--                        -> IO ThreadId
-- maintainPendingResChan pendingResChan responseTime threadDel workerId =
--     forkIO $ forever $ do
--     msg <- atomically $ readTChan pendingResChan
--     let keys = Map.keys msg
--     mapM_ (removeIfExpired pendingResChan responseTime msg) keys
--     threadDelay threadDel
-- extractDistance :: T.NodeId
--                 -> (T.NodeId,T.NodeEndPoint)
--                 -> ((T.NodeId,T.NodeEndPoint),Int)
-- extractDistance localNodeId x  = (x,kbi)
--     where temp = fst x       :: PublicKey
--           nid  = localNodeId :: PublicKey
--           dis  = Data.ByteArray.xor temp nid :: C.ByteString
--           kbi  = I# (integerLog2# (bs2i dis))
