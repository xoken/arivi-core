module Network.Arivi.Datagram
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


import qualified Network.Arivi.Multiplexer as MP
import qualified Network.Arivi.Types       as T

runUDPServerForever :: Socket
                    -> SockAddr
                    -> MP.Registry
                    -> IO ()

runUDPServerForever sock sockAddr registry = do

    bind sock sockAddr
    print "Server now listening for requests at : "
    forever $
                do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            -- lookup the protocol
            -- get corresponding callback say callb
            -- pass msg to callback
            -- callb (mesg,socadr2)
            print ""




