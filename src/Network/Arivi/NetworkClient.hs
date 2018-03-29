module Network.Arivi.NetworkClient
(
    networkClient
) where

import           Control.Concurrent                         (forkIO,ThreadId,newEmptyMVar,putMVar,
                                                            takeMVar)
import           Control.Concurrent.MVar
import           Control.Monad                              (forever)
import           Network.Socket
import           Control.Concurrent.STM                     (atomically,TChan,TMVar,newTMVar,
                                                            newTChan,writeTChan,readTChan,
                                                            readTMVar)
import qualified Network.Socket.ByteString          as N    (recvFrom,sendTo)
import qualified Data.ByteString.Char8              as C
import qualified Data.List.Split                    as S
import           Data.Word
import           Data.Maybe                                  (fromMaybe)
import qualified Data.Map.Strict                    as Map


networkClient :: TChan (C.ByteString,SockAddr)
              -> TMVar (SockAddr,Socket)
              -> Int
              -> IO ThreadId

networkClient outboundChan socketTMVar workerId = forkIO $ forever $ do
    sockMsg <- atomically $ readTMVar socketTMVar
    msg     <- atomically $ readTChan outboundChan
    let pl = fst msg

    N.sendTo (snd sockMsg) (pl) (fst sockMsg)
