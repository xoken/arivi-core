module Arivi.Network.NetworkClient
(
    datagramClient,
    streamClient
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


datagramClient :: TChan (C.ByteString,SockAddr)
              -> (Socket,SockAddr)
              -> Int
              -> IO ThreadId

datagramClient outboundDatagramTChan socketTMVar workerId = undefined

streamClient :: TChan (C.ByteString,SockAddr)
              -> (Socket,SockAddr)
              -> Int
              -> IO ThreadId

streamClient outboundStreamTChan socketTMVar workerId = undefined

