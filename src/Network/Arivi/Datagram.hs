module Network.Arivi.Datagram  
(
    runUDPServerForever,
    networkClient,
    SockAddr,
    Socket,
    PortNumb,
    HostAddr,
    Payload (..),
    AriviConfig (..),
    runAriviInstance

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

runUDPServerForever :: SockAddr
                    -> Socket  
                    -> MP.Registry 
                    -> IO ThreadId 

runUDPServerForever sockAddr sock messageHandler = do
    
    bind sock sockAddr 
    -- printing is here just for testing purposes and won't be required after full integration 
    print ("Server now listening for requests at : " ++ local_port)
    putStrLn ""
    forkIO $ forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            atomically $ writeTChan inboundChan (mesg,socaddr2)


