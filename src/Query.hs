{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}

module Query 
(
queryKBucket 
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
import           Utils              
import qualified Types as T 

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

-- Helper function to extract "k-closest" peers from a given list of keys 
getPeerListFromKeyList :: [Int] 
                       -> Int 
                       -> (Map.Map Int [(T.NodeId,T.NodeEndPoint)]) 
                       -> [(T.NodeId,T.NodeEndPoint)]

getPeerListFromKeyList [] k msg = [] 
getPeerListFromKeyList (x:xs) k msg 
    | (ls >= k) = fst (Prelude.splitAt k plt)
    | otherwise = pl ++ getPeerListFromKeyList xs (k - (Prelude.length (pl))) msg
  
    where 
        ls   = Prelude.length (Map.lookup x msg)
        pl   = fst (Prelude.splitAt k plt)
        plt  = fromMaybe [] (Map.lookup x msg)

-- Function responsible for querying K-buckets to return K-closest peers to FIND_NODE issuer 
queryKBucket :: T.NodeId 
             -> T.NodeId 
             -> Int  
             -> TChan (Map.Map Int [(T.NodeId,T.NodeEndPoint)]) 
             -> TChan (T.PayLoad,SockAddr,Socket)
             -> SockAddr
             -> SockAddr
             -> Socket 
             -> SecretKey 
             -> IO ()

queryKBucket localNodeId targetNodeId k kbChan outboundChan localSock remoteSock localSocket sk = do 
    let dis = (Data.ByteArray.xor (localNodeId :: PublicKey) (targetNodeId :: PublicKey)) :: C.ByteString
        kbi = I# (integerLog2# (bs2i dis))  
    
    msg <- atomically $ peekTChan kbChan
    let keys = Map.keys msg
        temp = [x | x <- keys]

    let peerList = getPeerListFromKeyList temp k msg 
    
    -- Payload which is actually response for FIND_NODE X 
    let msgType  = T.MSG04 
        fromep   = T.NodeEndPoint (sockAddrToHostAddr localSock) (sockAddrToPortNumber localSock) (sockAddrToPortNumber localSock)
        seq      = 1
        msgbody  = T.FN_RESP localNodeId peerList fromep
    ts <- T.getTimeStamp 
    let msgS     = T.Message msgType msgbody seq ts 
        sgn      = (sign (sk) (localNodeId :: PublicKey) (LBS.toStrict (serialise(msgS)) )) :: T.Sign 
        payl     = T.PayLoad msgS sgn            
    atomically $ writeTChan outboundChan (payl,remoteSock,localSocket)
            