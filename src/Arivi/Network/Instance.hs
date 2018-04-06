
module Arivi.Network.Instance
(
AriviConfig (..),
getAriviInstance ,
runAriviInstance ,
AriviHandle (..),
KI.Config (..)
) where

import qualified Arivi.Kademlia.Instance   as KI
import           Arivi.Network.Datagram
import qualified Arivi.Network.Multiplexer as MP
import           Arivi.Network.Stream
import           Arivi.Network.Types
import           Control.Concurrent        (MVar, ThreadId, forkIO,
                                            newEmptyMVar, putMVar, readMVar,
                                            takeMVar)
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Map.Strict           as Map
import           Network.Socket


-- | Strcuture to hold the arivi configurations can also contain more
--   parameters but for now just contain 3
data AriviConfig    = AriviConfig {
                        hostip  :: String
                    ,   udpport :: String
                    ,   tcpPort :: String
                    } deriving (Show)

-- | Strcuture which holds all the information about a running arivi Instance
--   and can be passed around to different functions to differentiate betweeen
--   different instances of arivi.
data AriviHandle    = AriviHandle {
                        ariviUDPSock :: (Socket,SockAddr)
                    ,   ariviTCPSock :: (Socket,SockAddr)
                    ,   udpThread    :: MVar ThreadId
                    ,   tcpThread    :: MVar ThreadId
                    ,   registry     :: MP.Registry
            }

getAriviInstance :: AriviConfig -> IO AriviHandle
getAriviInstance ac = do
    addrinfos <- getAddrInfo Nothing (Just (hostip ac)) (Just (udpport ac))
    let udpServeraddr = head addrinfos
        tcpServeraddr = head $ tail addrinfos

    udpSock <- socket (addrFamily udpServeraddr) Datagram defaultProtocol
    tcpSock <- socket (addrFamily tcpServeraddr) Stream defaultProtocol

    let ariviUdpSock = (udpSock,addrAddress udpServeraddr)
        arivitcpSock = (tcpSock,addrAddress tcpServeraddr)
        registry     = MP.Registry Map.empty
    udpt <- newEmptyMVar
    tcpt <- newEmptyMVar
    return (AriviHandle ariviUdpSock arivitcpSock udpt tcpt registry)

-- | Starts an arivi instance from ariviHandle which contains all the
--   information required to run an arivi instance.
runAriviInstance :: AriviHandle
                 -> KI.Config
                 -> IO ()

runAriviInstance ah kcfg = do
    tid1 <- async $ uncurry runUDPServerForever (ariviUDPSock ah) (registry ah)
    tid2 <- async $ uncurry runTCPServerForever (ariviTCPSock ah) (registry ah)

    let threadIDUDP = asyncThreadId tid1
        threadIDTCP = asyncThreadId tid2
    putMVar (udpThread ah) threadIDUDP
    putMVar (tcpThread ah) threadIDTCP

    wait tid1
    wait tid2

-- | Register callback functions for subprotocols which will be fired when
--   arivi recieves a message meant for a particular subprotocl essentially
--   passing the control to subprotocol with the message.

registerService :: AriviHandle
                -> (PayLoad -> IO())
                -> SubProtocol
                -> ServiceContext

registerService ah callback subprotocol = undefined

-- | assigns a unique session for each connection & subprotocol between two
--   nodes which is identified by a sessionID

createSession :: ServiceContext
              -> KI.NodeId
              -> PortNumber
              -> HostAddress
              -> TransportType
              -> SessionId

createSession sc nid pnum haddr tp = undefined

getSession :: ServiceContext
           -> KI.NodeId
           -> SessionId

getSession sc nid = undefined

sendMessage :: SessionId -> PayLoad -> IO ()
sendMessage ssid message = undefined

closeSession :: SessionId -> IO ()
closeSession ssid = undefined
