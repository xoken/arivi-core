module Network.Arivi.Instance
(

) where 

import           Network.Arivi.Types 
import           Control.Concurrent                         (forkIO,ThreadId,newEmptyMVar,putMVar,
                                                            takeMVar,MVar)
import qualified Data.Map.Strict                    as Map 
                                                            



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
                    ,   registry        :: Map.Map Int (Payload -> IO())   
            }  
