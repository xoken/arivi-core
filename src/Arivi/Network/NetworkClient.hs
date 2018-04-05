module Arivi.Network.NetworkClient
(
    networkClient
) where

import           Control.Concurrent        (ThreadId, forkIO, newEmptyMVar,
                                            putMVar, takeMVar)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM    (TChan, TMVar, atomically, newTChan,
                                            newTMVar, readTChan, readTMVar,
                                            writeTChan)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import qualified Data.List.Split           as S
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString as N (recvFrom, sendTo)


networkClient :: TChan (C.ByteString,SockAddr)
              -> TMVar (SockAddr,Socket)
              -> Int
              -> IO ThreadId

networkClient outboundChan socketTMVar workerId = forkIO $ forever $ do
    sockMsg <- atomically $ readTMVar socketTMVar
    msg     <- atomically $ readTChan outboundChan
    let pl = fst msg

    N.sendTo (snd sockMsg) pl (fst sockMsg)
