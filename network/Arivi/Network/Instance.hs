module Arivi.Network.Instance
(
NetworkConfig (..),
-- getAriviInstance ,
-- runAriviInstance ,
NetworkHandle (..),
AriviNetworkInstance (..),
connectionMap,
mkAriviNetworkInstance,
openConnection,
sendMessage,
closeConnection
) where

import           Arivi.Env
import           Arivi.Network.Connection     as Conn (Connection (..),
                                                       makeConnectionId)
import qualified Arivi.Network.FSM            as FSM
import           Arivi.Network.StreamClient
import           Arivi.Network.Types          as ANT (ConnectionId, Event (..),
                                                      NodeId, OutboundFragment,
                                                      Parcel, Payload (..),
                                                      PersonalityType,
                                                      TransportType (..))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan (TChan)
import           Control.Monad.Reader
import           Control.Monad.STM            (atomically)
import           Data.ByteString.Lazy
import           Data.HashMap.Strict          as HM
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
newtype NetworkHandle = NetworkHandle { ariviUDPSock :: (Socket,SockAddr) }
                    -- ,   ariviTCPSock :: (Socket,SockAddr)
                    -- ,   udpThread    :: MVar ThreadId
                    -- ,   tcpThread    :: MVar ThreadId
                    -- ,
                    -- registry     :: MVar MP.ServiceRegistry


openConnection :: (HasAriviNetworkInstance m) => HostAddress -> PortNumber -> TransportType -> NodeId -> PersonalityType -> m ANT.ConnectionId
openConnection addr port tt rnid pType = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  eventChan <- liftIO $ (newTChanIO :: IO (TChan Event))
  socket <- liftIO $ createSocket (show addr) (read (show port)) tt
  outboundChan <- liftIO $ (newTChanIO :: IO (TChan OutboundFragment))
  reassemblyChan <- liftIO $ (newTChanIO :: IO (TChan Parcel))
  let cId = makeConnectionId addr port tt
      connection = Connection {connectionId = cId, remoteNodeId = rnid, ipAddress = addr, port = port, transportType = tt, personalityType = pType, Conn.socket = socket, eventTChan = eventChan, outboundFragmentTChan = outboundChan, reassemblyTChan = reassemblyChan}
  liftIO $ atomically $  modifyTVar tv (HM.insert cId connection)
  liftIO $ withAsync (FSM.initFSM connection) (\_ -> atomically $ modifyTVar tv (HM.delete cId))
  return cId

sendMessage :: (HasAriviNetworkInstance m) => ANT.ConnectionId -> ByteString -> m ()
sendMessage cId msg = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  hmap <- liftIO $ readTVarIO tv
  let conn = case (HM.lookup cId hmap) of
        Just c -> c
        Nothing -> error "Something terrible happened! You have been warned not to enter the forbidden lands"
  liftIO $ atomically $ writeTChan (eventTChan conn) (SendDataEvent (Payload msg))

closeConnection :: (HasAriviNetworkInstance m) => ANT.ConnectionId -> m ()
closeConnection cId = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  hmap <- liftIO $ readTVarIO tv
  let conn = case (HM.lookup cId hmap) of
        Just c -> c
        Nothing -> error "Something terrible happened! You have been warned not to enter the forbidden lands"
  liftIO $ atomically $ modifyTVar tv (HM.delete cId)
