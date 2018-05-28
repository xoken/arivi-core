module Arivi.Network.NetworkClient
(
    datagramClient,
    streamClient
) where

import           Control.Concurrent     (ThreadId)
import           Control.Concurrent.STM (TChan)
import qualified Data.ByteString.Char8  as C
import           Network.Socket

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

