module Arivi.Network.Stream
(
    runTCPServerForever
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
import qualified Arivi.Network.Multiplexer          as MP    (Registry)


runTCPServerForever :: Socket
                    -> SockAddr
                    -> MP.Registry
                    -> IO ()

runTCPServerForever sock sockAddr messageHandler = do
    bind sock sockAddr
    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print ""
