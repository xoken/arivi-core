module Network.Arivi.Types 
(
AriviConfig (..),
AriviHandle (..),
Payload     (..)
) where 

import qualified Network.Socket.ByteString          as N    (recvFrom,sendTo) 
import qualified Data.ByteString.Char8              as C 
import qualified Data.Map.Strict                    as Map 
import           Control.Concurrent                         (forkIO,ThreadId,newEmptyMVar,putMVar,
                                                            takeMVar)

-- | Structure to hold the Payload which arivi will send and receive (PENDING)
data Payload = Payload C.ByteString 
               deriving (Show)

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