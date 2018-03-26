module Network.Arivi.Types 
(

) where 

import qualified Network.Socket.ByteString          as N    (recvFrom,sendTo) 
import qualified Data.ByteString.Char8              as C 
import qualified Data.Map.Strict                    as Map 
import           Control.Concurrent                         (forkIO,ThreadId,newEmptyMVar,putMVar,
                                                            takeMVar)
data AriviConfig    = AriviConfig {
                        hostip  :: String 
                    ,   udpport :: String 
                    ,   tcpPort :: String 
                    } deriving (Show)

data AriviHandle    = AriviHandle {
                        ariviUdpSock    :: (Socket,SockAddr)  
                    ,   ariviTcpSock    :: (Socket,SockAddr)
                    ,   udpThread       :: MVar ThreadId  
                    ,   tcpThread       :: MVar ThreadId    
                    ,   registry        :: Map Int (Payload -> IO())   
                    }  