module Arivi.Network.Stream
(
    runTCPServerForever
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
import           Data.Maybe                (fromMaybe)
import           Data.Word
import           Network.Socket
import qualified Network.Socket.ByteString as N (recvFrom, sendTo)


runTCPServerForever :: Socket
                    -> SockAddr
                    -> IO ()

runTCPServerForever sock sockAddr = do
    bind sock sockAddr
    listen sock 3
    --  int above specifies the maximum number of queued connections and should
    --  be at least 1; the maximum value is system-dependent (usually 5).

    print ("TCP Server now listening for requests at : " ++ show sockAddr)
    forever $
         do
            (conn,addr) <- accept sock
            (mesg, socaddr2) <- N.recvFrom sock 4096
            close conn
            close sock
