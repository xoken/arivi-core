
module Arivi.Network.Instance
(
NetworkConfig (..),
-- getAriviInstance ,
-- runAriviInstance ,
NetworkHandle (..),
KI.Config (..)
) where

import qualified Arivi.Kademlia.Instance      as KI
import           Arivi.Network.Connection     (Connection (..),
                                               makeConnectionId)
import           Arivi.Network.Datagram
-- import qualified Arivi.Network.Multiplexer    as MP
import qualified Arivi.Network.FSM            as FSM
import           Arivi.Network.NetworkClient
import           Arivi.Network.Stream
import           Arivi.Network.Types
import           Arivi.P2P.Types              (ServiceRequest (..),
                                               ServiceType (..))
import           Arivi.Utils.Utils
import           Control.Concurrent           (MVar, ThreadId, forkIO,
                                               newEmptyMVar, newMVar, putMVar,
                                               readMVar, swapMVar, takeMVar)
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan (TChan, newTChan)
import           Control.Monad
import           Control.Monad.STM            (atomically)
import           Data.Int
import qualified Data.Map.Strict              as Map
import           Network.Socket


-- | Strcuture to hold the arivi configurations can also contain more
--   parameters but for now just contain 3
data NetworkConfig    = NetworkConfig {
                        hostip  :: String
                    ,   udpport :: String
                    ,   tcpPort :: String
                    -- , TODO   transportType :: TransportType and only one port
                    } deriving (Show)

-- | Strcuture which holds all the information about a running arivi Instance
--   and can be passed around to different functions to differentiate betweeen
--   different instances of arivi.
newtype NetworkHandle    = NetworkHandle {
                        ariviUDPSock :: (Socket,SockAddr)
                    -- ,   ariviTCPSock :: (Socket,SockAddr)
                    -- ,   udpThread    :: MVar ThreadId
                    -- ,   tcpThread    :: MVar ThreadId
                    -- ,
                    -- registry     :: MVar MP.ServiceRegistry
            }

-- getAriviInstance :: NetworkConfig -> IO NetworkHandle
-- getAriviInstance ac = withSocketsDo $ do
--     addrinfos <- getAddrInfo Nothing (Just (hostip ac)) (Just (udpport ac))
--     let udpServeraddr = head addrinfos
--         tcpServeraddr = head $ tail addrinfos

--     udpSock <- socket (addrFamily udpServeraddr) Datagram defaultProtocol
--     tcpSock <- socket (addrFamily tcpServeraddr) Stream defaultProtocol

--     let ariviUdpSock = (udpSock,addrAddress udpServeraddr)
--         arivitcpSock = (tcpSock,addrAddress tcpServeraddr)

--     udpt     <- newEmptyMVar
--     tcpt     <- newEmptyMVar
--     registry <- newMVar $ MP.ServiceRegistry Map.empty

--     return (AriviHandle ariviUdpSock arivitcpSock udpt tcpt registry)

-- | Starts an arivi instance from ariviHandle which contains all the
--   information required to run an arivi instance.
-- runAriviInstance :: NetworkHandle
--                  -> KI.Config
--                  -> IO ()

-- runAriviInstance ah kcfg = do
--     tid1 <- async $ uncurry runUDPServerForever (ariviUDPSock ah) (registry ah)
--     tid2 <- async $ uncurry runTCPServerForever (ariviTCPSock ah) (registry ah)

--     let threadIDUDP = asyncThreadId tid1
--         threadIDTCP = asyncThreadId tid2
--     putMVar (udpThread ah) threadIDUDP
--     putMVar (tcpThread ah) threadIDTCP

--     ki <- KI.createKademliaInstance kcfg (fst $ ariviUDPSock ah)
--     tid3 <- async $ KI.runKademliaInstance ki

--     outboundDatagramTChan <- atomically newTChan
--     outboundStreamTChan   <- atomically newTChan

--     mapM_ (datagramClient outboundDatagramTChan (ariviUDPSock ah))  [1..10]
--     mapM_ (streamClient outboundStreamTChan (ariviTCPSock ah)) [1..10]

--     wait tid1
--     wait tid2
--     wait tid3

-- | Register service provides a service context to each application by
--   creating two Chans i.e inbound and outbound Chan and registering
--   them corresponding to a ServiceCode unique to each application
--   therefore when Arivi Network protocol recieves any messages meant for
--   a service it writes the message to these chans and application can then
--   read these chans to read the message.

-- registerService :: AriviHandle
--                 -> ServiceCode
--                 -> IO ServiceContext

-- registerService ah sc = do
--     inboundChan  <- atomically newTChan
--     outboundChan <- atomically newTChan
--     reg <- readMVar $ registry ah
--     let chanTuple = (inboundChan,outboundChan)
--         temp = MP.ServiceRegistry $ Map.insert sc chanTuple
--                     (MP.serviceRegistry temp)

--     swapMVar (registry ah ) temp
--     getRandomSequence2


-- | TODO Called by TCP Server
-- handleNewIncomingConnection frameTChan = undefined
-- connectionhNADLER

-- | TODO create connection and spawns FSM by passing connection
initiateConnection ipAddress port nodeId transportType peerType = do
                        -- create connection
                        connectionId <-  makeConnectionId ipAddress port
                                                          transportType

                        let connection = Connection connectionId nodeId
                                                    ipAddress port
                                                    nodeId undefined
                                                    transportType peerType
                                                    undefined undefined
                                                    undefined undefined
                                                    undefined undefined
                        -- pass to init fsm
                        -- let serviceRequest = DataServiceRequest OPEN
                        -- fsmHandle <- async (FSM.handleEvent connection FSM.Idle
                        --             (FSM.InitHandshakeEvent serviceRequest))
                        -- wait fsmHandle
                        -- returns connection
                        return connection

-- recvMessages :: ServiceContext -> IO PayLoad
-- recvMessages sc = undefined

-- sendMessage :: SessionId -> PayLoad -> IO ()
-- sendMessage ssid message = undefined

-- closeSession :: SessionId -> IO ()
-- closeSession ssid = undefined
