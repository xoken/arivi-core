module Arivi.Network.Stream
(
    runTCPServerForever
) where

import qualified Arivi.Network.Multiplexer as MP (Registry)
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
                    -> MP.Registry
                    -> IO ()

runTCPServerForever sock sockAddr messageHandler = do
    bind sock sockAddr
    print ("TCP Server now listening for requests at : " ++ show (sockAddr))
    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print ""
