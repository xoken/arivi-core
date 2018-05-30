{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}

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
, lookupCId
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection             as Conn (Connection (..),
                                                               makeConnectionId)
import qualified Arivi.Network.FSM                    as FSM
import           Arivi.Network.StreamClient
import           Arivi.Network.StreamServer
import           Arivi.Network.Types                  as ANT (ConnectionId,
                                                              Event (..),
                                                              NodeId,
                                                              OutboundFragment,
                                                              Parcel,
                                                              Payload (..),
                                                              PersonalityType,
                                                              TransportType (..))
import           Control.Concurrent.Async.Lifted.Safe
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan         (TChan)
import           Control.Monad.Reader
import           Control.Monad.STM                    (atomically)
import           Data.ByteString.Lazy
import           Data.HashMap.Strict                  as HM
import           Data.Maybe                           (fromMaybe)
import           Network.Socket

import           Debug.Trace


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
-- openConnection1 :: HostAddress -> PortNumber -> TransportType -> NodeId -> PersonalityType -> _ -> IO ANT.ConnectionId
-- openConnection1 addr port tt rnid pType sk = do
--   let socket = "abc"
--   socket <- createSocket (show addr) (read (show port)) tt
--   print socket
--   eventChan <- liftIO (newTChanIO :: IO (TChan Event))
--   outboundChan <- (newTChanIO :: IO (TChan OutboundFragment))
--   reassemblyChan <- (newTChanIO :: IO (TChan Parcel))
--   let cId = makeConnectionId addr port tt
--       connection = Connection {connectionId = cId, remoteNodeId = rnid, ipAddress = addr, port = port, transportType = tt, personalityType = pType, eventTChan = eventChan, outboundFragmentTChan = outboundChan, reassemblyTChan = reassemblyChan}
--   -- $(withLoggingTH) (LogNetworkStatement "Spawning FSM") LevelInfo $ withAsync (FSM.initFSM connection) (\_ -> liftIO $ atomically $ modifyTVar tv (HM.delete cId))
--   print "HEHEH"
--   -- $(withLoggingTH) (LogNetworkStatement "Spawning FSM") LevelInfo $ async (FSM.initFSM connection)
--   -- _ <- async (liftIO $ readSock socket eventChan sk)
--   return cId

openConnection :: (HasAriviNetworkInstance m,
                   HasSecretKey m,
                   HasLogging m,
                   Forall (Pure m))
               => HostName
               -> PortNumber
               -> TransportType
               -> NodeId
               -> PersonalityType
               -> m (ANT.ConnectionId)
openConnection addr port tt rnid pType = do
  ariviInstance <- getAriviNetworkInstance
  let tv = connectionMap ariviInstance
  sk <- getSecretKey
  eventChan <- liftIO (newTChanIO :: IO (TChan Event))
  socket <- liftIO $ createSocket addr (read (show port)) tt
  outboundChan <- liftIO (newTChanIO :: IO (TChan OutboundFragment))
  reassemblyChan <- liftIO (newTChanIO :: IO (TChan Parcel))
  p2pMsgTChan <- liftIO (newTChanIO :: IO (TChan ByteString))
  let cId = makeConnectionId addr port tt
      connection = Connection {connectionId = cId, remoteNodeId = rnid, ipAddress = addr, port = port, transportType = tt, personalityType = pType, Conn.socket = socket, eventTChan = eventChan, outboundFragmentTChan = outboundChan, reassemblyTChan = reassemblyChan, p2pMessageTChan = p2pMsgTChan}

  liftIO $ atomically $ modifyTVar tv (HM.insert cId connection)
  liftIO $ atomically $ writeTChan eventChan (InitHandshakeEvent sk)
  tid <- $(withLoggingTH) (LogNetworkStatement "Spawning FSM") LevelInfo $ async (FSM.initFSM connection) -- (\a -> do wait a)

  -- $(withLoggingTH) (LogNetworkStatement "Spawning FSM") LevelInfo $ async (FSM.initFSM connection)
  _ <- async (liftIO $ readSock socket eventChan sk)
  hm <- liftIO $ readTVarIO tv
  traceShow ("TTTTT " ++ (show $ HM.size hm)) (return ())
  return (cId)

sendMessage :: (HasAriviNetworkInstance m)
            => ANT.ConnectionId
            -> ByteString
            -> m ()
sendMessage cId msg = do
  conn <- lookupCId cId
  liftIO $ atomically $ writeTChan (eventTChan conn) (SendDataEvent (Payload msg))

closeConnection :: (HasAriviNetworkInstance m)
                => ANT.ConnectionId
                -> m ()
closeConnection cId = do
  ariviInstance <- getAriviNetworkInstance
  let tv = connectionMap ariviInstance
  liftIO $ atomically $ modifyTVar tv (HM.delete cId)

lookupCId :: (HasAriviNetworkInstance m)
          => ANT.ConnectionId
          -> m Connection
lookupCId cId = do
  ariviInstance <- getAriviNetworkInstance
  let tv = connectionMap ariviInstance
  hmap <- liftIO $ readTVarIO tv
  traceShow ("SSSS " ++ (show $ HM.size hmap)) (return ())
  return $ fromMaybe (error "Something terrible happened! You have been warned not to enter the forbidden lands") (HM.lookup cId hmap)
