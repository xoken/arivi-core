module Network.Arivi.Instance
(

) where 

import           Network.Arivi.Types 
import           Control.Concurrent                         (forkIO,ThreadId,newEmptyMVar,putMVar,
                                                            takeMVar,MVar)
import qualified Data.Map.Strict                    as Map 
import qualified Network.Arivi.Multiplexer          as MP 
                                                            



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
                    ,   registry        :: Map.Map Int (PayLoad -> IO())   
            }  


getAriviInstance :: AriviConfig -> AriviHandle 
getAriviInstance ac = undefined 

registerCallback :: Int 
                 -> (PayLoad -> IO())
                 -> Bool 
registerCallback key value = undefined 

runAriviInstance :: AriviHandle 
                 -> IO () 
runAriviInstance ah = undefined 

