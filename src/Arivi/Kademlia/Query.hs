{-# LANGUAGE MagicHash #-}

module Arivi.Kademlia.Query
(
queryKBucket,
isNodeInKbucket
 ) where

import qualified Arivi.Kademlia.Types         as T
import           Arivi.Utils.Utils
import           Control.Concurrent           (Chan, ThreadId, forkIO,
                                               isEmptyChan, newChan,
                                               newEmptyMVar, putMVar, readChan,
                                               takeMVar, threadDelay, writeChan)
import           Control.Concurrent.STM.TChan
import           Control.Monad                (forever)
import           Control.Monad.STM
import           Crypto.PubKey.Ed25519
import           Crypto.Util
import           Data.ByteArray
import           Data.ByteString.Base16       as H
import qualified Data.ByteString.Char8        as C (ByteString)
import qualified Data.ByteString.Lazy         as LBS
import           Data.List                    as L
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Word                    (Word32)
import           GHC.Exts
import           GHC.Integer.Logarithms
import           Network.Socket
import qualified Network.Socket.ByteString    as N (recv, recvFrom, sendAll,
                                                    sendAllTo, sendTo)

-- | Helper function to extract "k-closest" peers from a given list of keys
getPeerListFromKeyList :: [Int]
                       -> Int
                       -> Map.Map Int [(T.NodeId,T.NodeEndPoint)]
                       -> [(T.NodeId,T.NodeEndPoint)]

getPeerListFromKeyList [] k msg = []
getPeerListFromKeyList (x:xs) k msg
    | ls >= k   = fst (Prelude.splitAt k plt)
    | otherwise = pl ++ getPeerListFromKeyList xs
                    (k - Prelude.length pl) msg

    where
        ls   = Prelude.length (Map.lookup x msg)
        pl   = fst (Prelude.splitAt k plt)
        plt  = fromMaybe [] (Map.lookup x msg)

-- | Checks if NodeId is present in KBuckets
isNodeInKbucket :: TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
                -> T.NodeId
                -> Int
                -> IO Bool

isNodeInKbucket kbChan nodeId kbi = do
    msg <- atomically $ peekTChan kbChan
    let temp  = Map.lookup kbi msg
        temp2 = fromMaybe [] temp
        temp3 = Prelude.map fst temp2

    if nodeId `elem` temp3 then return True else return False

-- | Function responsible for querying K-buckets to return K-closest peers to
--   FIND_NODE issuer
queryKBucket :: T.NodeId
             -> T.NodeId
             -> Int
             -> TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)])
             -> SockAddr
             -> SockAddr
             -> SecretKey
             -> Word32
             -> IO ()

queryKBucket localNodeId targetNodeId k kbChan localSock remoteSock sk seq = do
    let dis = Data.ByteArray.xor (localNodeId :: PublicKey)
                (targetNodeId :: PublicKey) :: C.ByteString
        kbi = I# (integerLog2# (bs2i dis))

    msg <- atomically $ peekTChan kbChan
    let keys = Map.keys msg
        temp = keys

    let peerList2    = getPeerListFromKeyList temp k msg
        tempfromep   = T.NodeEndPoint (sockAddrToHostAddr localSock)
                        (sockAddrToPortNumber localSock)
                        (sockAddrToPortNumber localSock)
        peerList     = L.deleteBy (\(x,y) (a,b) -> a==x)
                        (targetNodeId,tempfromep) peerList2

    -- Payload which is actually response for FIND_NODE X
    let msgType  = T.MSG04
        fromep   = T.NodeEndPoint (sockAddrToHostAddr localSock)
                    (sockAddrToPortNumber localSock)
                    (sockAddrToPortNumber localSock)
        msgbody  = T.FN_RESP localNodeId peerList fromep
    let msgS     = T.Message msgType msgbody seq
        sgn      = sign sk (localNodeId :: PublicKey)
                    (LBS.toStrict (T.serialise msgS) ) :: T.Sign
        payl     = T.PayLoad msgS sgn

    --  Arivi.send (payl,remoteSock)
    print ""

