module Network.Arivi.Datagram
(
    runUDPServerForever
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


import qualified Network.Arivi.Types                as T
import qualified Network.Arivi.Multiplexer          as MP

runUDPServerForever :: Socket
                    -> SockAddr
                    -> MP.Registry
                    -> IO ()

runUDPServerForever sock sockAddr registry = do

    bind sock sockAddr
    print ("Server now listening for requests at : ")
    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            -- lookup the protocol
            -- get corresponding callback say callb
            -- pass msg to callback
            -- callb (mesg,socadr2)
            print ""




