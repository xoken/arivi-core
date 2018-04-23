module Arivi.Network.Datagram
(
    runUDPServerForever
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


import qualified Arivi.Network.Types       as T

runUDPServerForever :: Socket
                    -> SockAddr
                    -> IO ()

runUDPServerForever sock sockAddr  = do

    bind sock sockAddr
    print ("UDP Server now listening for requests at : " ++ show sockAddr)
    forever $
                do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print ""
