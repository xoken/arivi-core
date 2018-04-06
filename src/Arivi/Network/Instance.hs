
module Arivi.Network.Instance
(
AriviConfig (..),
getAriviInstance ,
runAriviInstance ,
AriviHandle (..),
KI.Config (..)
) where

import qualified Arivi.Kademlia.Instance      as KI
import           Arivi.Network.Datagram
import qualified Arivi.Network.Multiplexer    as MP
import           Arivi.Network.NetworkClient
import           Arivi.Network.Stream
import           Arivi.Network.Types
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
                    ,   registry     :: MVar MP.ServiceRegistry
            }

getAriviInstance :: AriviConfig -> IO AriviHandle
getAriviInstance ac = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just (hostip ac)) (Just (udpport ac))
    let udpServeraddr = head addrinfos
        tcpServeraddr = head $ tail addrinfos

    udpSock <- socket (addrFamily udpServeraddr) Datagram defaultProtocol
    tcpSock <- socket (addrFamily tcpServeraddr) Stream defaultProtocol

    let ariviUdpSock = (udpSock,addrAddress udpServeraddr)
        arivitcpSock = (tcpSock,addrAddress tcpServeraddr)

    udpt     <- newEmptyMVar
    tcpt     <- newEmptyMVar
    registry <- newMVar $ MP.ServiceRegistry Map.empty

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

    ki <- KI.createKademliaInstance kcfg (fst $ ariviUDPSock ah)
    tid3 <- async $ KI.runKademliaInstance ki

    outboundDatagramTChan <- atomically newTChan
    outboundStreamTChan   <- atomically newTChan

    mapM_ (datagramClient outboundDatagramTChan (ariviUDPSock ah))  [1..10]
    mapM_ (streamClient outboundStreamTChan (ariviTCPSock ah)) [1..10]

    wait tid1
    wait tid2
    wait tid3

-- | Register callback functions for subprotocols which will be fired when
--   arivi recieves a message meant for a particular subprotocl essentially
--   passing the control to subprotocol with the message.

registerService :: AriviHandle
                -> (PayLoad -> IO())
                -> ServiceId
                -> IO ServiceContext

registerService ah callback sid = do
    temp <- readMVar $ registry ah
    let temp2 = Map.insert sid callback (MP.serviceRegistry temp)
        temp3 = MP.ServiceRegistry temp2
    swapMVar (registry ah) temp3
    getRandomSequence2

sendMessage :: SessionId -> PayLoad -> IO ()
sendMessage ssid message = undefined

closeSession :: SessionId -> IO ()
closeSession ssid = undefined




