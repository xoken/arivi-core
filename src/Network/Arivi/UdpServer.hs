{-# LANGUAGE RankNTypes #-}

module Network.Arivi.UdpServer 
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
                                                            takeMVar )
import           Control.Monad                              (forever)
import           Network.Socket                              
import           Control.Concurrent.STM                     (atomically,TChan,TMVar,newTMVar,
                                                            newTChan,writeTChan,readTChan,
                                                            readTMVar)
import qualified Network.Socket.ByteString          as N    (recvFrom,sendTo) 
import qualified Data.ByteString.Char8              as C 
import           Eve                                
import qualified Data.List.Split                    as S 
import           Data.Word 
import           Data.Maybe                                  (fromMaybe)


-- | Structure to hold the Payload which arivi will send and recieve (PENDING)
data Payload = Payload C.ByteString 
               deriving (Show)

-- | type synonyms for PortNumber and HostAdress, they are named this way because of the  
--   exisiting types in Network.Socket.    
type PortNumb      = String 
type HostAddr      = String 

-- | structure to hold arivi configuration and other parameters and using this config 
--   a new arivi instance can be created, this structure in it's current form doesn't 
--   contain all the configurations and more thought need to be put into it to decide on 
--   more useful parameters. 
data AriviConfig    = AriviConfig {
                        hostip  :: HostAddr
                    ,   udpport :: Maybe PortNumb
                    ,   tcpPort :: Maybe PortNumb  
                    } deriving (Show)

data AriviInstance  = AriviInstance {
                        ariviConfig :: AriviInstance 
                    } deriving (Show)
 
-- | Server 
--   Since Arivi is a multi protocol peer to peer solution it need to have both UDP and 
--   TCP server to send and recieve messages and it also becomes crucial to have two 
--   servers simultaenously running because different nodes can choose to use different protcol
--   and overall arivi is a abstraction over underlying protocol to user can send and recieve 
--   messages without worrying about the underlying protocol. 

-- | UDP Server, it constantly recieves requests and writes the incoming messages to the 
--   "InboundChan" which is a TChan, for further procressing. 
runUDPServerForever :: HostAddr
                    -> PortNumb
                    -> TChan(C.ByteString,SockAddr) 
                    -> IO ThreadId 

runUDPServerForever local_ip local_port inboundChan = do
    addrinfos <- getAddrInfo Nothing (Just local_ip) (Just local_port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)

    -- printing is here just for testing purposes and won't be required after full integration 
    print ("Server now listening for requests at : " ++ local_port)
    putStrLn ""
    forkIO $ forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            atomically $ writeTChan inboundChan (mesg,socaddr2) 

-- | Not yet implemented 
runTCPServerForever :: HostAddr
                    -> PortNumb
                    -> TChan(C.ByteString,SockAddr) 
                    -> IO ThreadId 

runTCPServerForever local_ip local_port inboundChan = undefined 


-- | the network client function simply reads a tChan called "OutboundChan" and sends out the 
--   content of tChan to the recipient mentioned in the tchan. The tchan contains a tuple which 
--   contains the message to sent and the recipient's SockAddr.   
networkClient :: TChan (C.ByteString,SockAddr)
              -> TMVar (SockAddr,Socket)
              -> Int 
              -> IO ThreadId

networkClient outboundChan socketTMVar workerId = forkIO $ forever $ do 
    sockMsg <- atomically $ readTMVar socketTMVar 
    msg     <- atomically $ readTChan outboundChan
    let pl = fst msg  

    N.sendTo (snd sockMsg) (pl) (fst sockMsg)

-------------------------------------------------------------------------------
-- | Event Driven Architecture 
-------------------------------------------------------------------------------

-- | since arivi handles the sending, recieving, framing , chunking etc and sits at a lower 
--   level of abstraction and the actuall processing of messages happens at much higher 
--   level therefore a mechanism is required to pass the messages recieved by arivi 
--   to upper application layer such as kademlia for peer discovert or block-syncing or for
--   block inventory etc. 

-- | We have taken the event driven approach wherein arivi exposes a function which 
--   accepts a callback function and under the hood arivi listens to an event which in 
--   this particular case is udpserver receiving a message and every time a message is 
--   recieved the event is triggered which further alerts the event listener and fires 
--   the callback function with received message and pass the control back to callback function 


-- | inboundMessageDispatcher function reads the inbound tchan to which UDP server writes 
--   and accepts a function which can then be called back upon arrival of message 
--   to process the message 
inboundMessageDispatcher :: TChan (C.ByteString,SockAddr) 
                         -> EventDispatcher 
                         -> IO ()

inboundMessageDispatcher inboundChan dispatcher = forever $ do 
    msg <- atomically $ readTChan inboundChan
    dispatcher (Payload $ fst msg)   

-- | exposes the eventProvider and adds listener 
registerCallback :: (Payload -> App () ) 
                 -> TChan (C.ByteString,SockAddr) 
                 -> App () 
registerCallback msgHandler inboundChan  = do 
    asyncEventProvider (inboundMessageDispatcher inboundChan)
    addListener_ msgHandler 

-- | Default portNumber in case fromMaybe fails 
defaultUdpPort :: PortNumb 
defaultUdpPort = "7000"


-- | forks a udp server on a seperate thread and along with arivi config also accepts a 
--   function which will be called everytime the server recieves a message along with the 
--   message  
runAriviInstance :: AriviConfig -> (Payload -> App ()) -> IO ()
runAriviInstance ariviConfig msgHandler = do 

    let ip   = hostip ariviConfig
        port = fromMaybe defaultUdpPort (udpport ariviConfig) 
    
    inboundChan <- atomically $ newTChan 
    runUDPServerForever ip port inboundChan  
    eve_ (registerCallback msgHandler inboundChan)


-- | gently stops the running arivi instance 
stopAriviInstance ah = undefined 
