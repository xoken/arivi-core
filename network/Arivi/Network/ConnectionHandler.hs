{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Arivi.Network.ConnectionHandler
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing dispatch of frames in
-- Arivi communication
module Arivi.Network.ConnectionHandler
(
     acceptIncomingSocket
   , getIPAddress
   , getPortNumber
   , getTransportType
   , handleInboundConnection
   , readHandshakeRespSock
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection        as Conn
import qualified Arivi.Network.FSM               as FSM (initFSM)
import           Arivi.Network.Handshake
import           Arivi.Network.StreamClient
import           Arivi.Network.Types
import           Debug.Trace
-- import           Arivi.Network.Types             (DeserialiseFailure,
--                                                   Event (..), Header (..),
--                                                   Parcel (..),
--                                                   deserialiseOrFail)
import           Arivi.Network.Reassembler
import           Arivi.Utils.Exception
import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.Async        as Async (async, race)
import qualified Control.Concurrent.Async.Lifted as LA (async)
import           Control.Concurrent.STM          (TChan, atomically, newTChan,
                                                  readTChan)
import           Control.Concurrent.STM.TVar
import           Control.Exception               (try)
import           Control.Exception.Base
import           Control.Monad                   (forever, void)
import           Control.Monad.IO.Class
import           Crypto.PubKey.Ed25519           (SecretKey)
import           Data.Binary
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy.Char8      as BSLC
import           Data.HashMap.Strict             as HM
import           Data.Int
import           Network.Socket
import qualified Network.Socket.ByteString       as N (recv)

-- | Reads encryptedPayload and socket from inboundTChan, constructs
-- connectionId using `makeConnectionId`. If this connectionId is already
-- present in the frameDispatchHashMap  then reads parcelTChan from
-- frameDispatchHashMap and writes this encryptedPayload in it, otherwise
-- creates new parcelTChan and writes encryptedPayload to it and stores
-- this (connectionId,parcelTChan) in the frameDispatchHashMap

-- inboundConnectionHandler :: TChan (Socket)
--      -> HashMap ConnectionId (TChan ByteString)
--      -> IO (HashMap ConnectionId (TChan ByteString))
handleInboundConnection :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => Socket -> m ()
handleInboundConnection mSocket = do
        sk <- getSecretKey
        ariviInstance <- getAriviNetworkInstance
        let tv = ariviNetworkConnectionMap ariviInstance
        let updatesTChan = ariviConnectionUpdatesTChan ariviInstance
        conn <- liftIO $ do
          socketName <- getSocketName mSocket
          mIpAddress <- inet_ntoa $ getIPAddress socketName
          let mPort = getPortNumber socketName
          let mTransportType = getTransportType mSocket
          let mConnectionId = Conn.makeConnectionId mIpAddress mPort mTransportType

          mReassemblyTChan <- atomically newTChan
          p2pMsgTChan <- atomically newTChan
          let connection = Conn.Connection { Conn.connectionId = mConnectionId
                                           , Conn.ipAddress = mIpAddress
                                           , Conn.port = mPort
                                           , Conn.transportType = mTransportType
                                           , Conn.personalityType = RECIPIENT
                                           , Conn.socket = mSocket
                                           , Conn.reassemblyTChan = mReassemblyTChan
                                           , Conn.p2pMessageTChan = p2pMsgTChan
                                           }
          -- getParcel, recipientHandshake and sendFrame might fail
          -- In any case, the thread just quits
          parcel <- readHandshakeInitSock mSocket
          (serialisedParcel, updatedConn) <- recipientHandshake sk connection parcel
          sendFrame (Conn.socket updatedConn) (createFrame serialisedParcel)
          atomically $ modifyTVar tv (HM.insert mConnectionId updatedConn)
          return updatedConn
        LA.async $ readSock conn HM.empty
        return ()

-- | Given `SockAddr` retrieves `HostAddress`
getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet _ hostAddress) = hostAddress
getIPAddress _                            = error "getIPAddress: SockAddr is not of constructor SockAddrInet "

-- | Given `SockAddr` retrieves `PortNumber`
getPortNumber :: SockAddr -> PortNumber
getPortNumber (SockAddrInet portNumber _) = portNumber
getPortNumber _                           = error "getPortNumber: SockAddr is not of constructor SockAddrInet "

-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ Stream _ _) = TCP
getTransportType _                         = UDP

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket :: (HasAriviNetworkInstance m, HasSecretKey m, HasLogging m) => Socket -> m a
acceptIncomingSocket sock = do
  sk <- getSecretKey
  forever $ do
        (mSocket, peer) <- liftIO $ accept sock
        liftIO $ putStrLn $ "Connection from " ++ show peer
        eventTChan <- liftIO $ atomically newTChan
        LA.async (handleInboundConnection mSocket) --or use forkIO



-- | Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Reads frame a given socket
getParcelWithTimeout :: Socket -> Int -> IO (Either AriviException Parcel)
getParcelWithTimeout sock timeout = do
    winner <- Async.race (threadDelay timeout) (N.recv sock 2)
    case winner of
      Left _ -> return $ Left AriviTimeoutException
      Right lenbs ->
        do
          parcelCipher <- N.recv sock $ getFrameLength lenbs
          either
            (return . Left . AriviDeserialiseException) (return . Right)
            (deserialiseOrFail (BSL.fromStrict parcelCipher))

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either AriviException Parcel)
getParcel sock = do
    lenbs <- N.recv sock 2
    parcelCipher <- N.recv sock $ getFrameLength lenbs
    either (return . Left . AriviDeserialiseException) (return . Right)
      (deserialiseOrFail (BSL.fromStrict parcelCipher))

sendPing :: Socket -> IO()
sendPing sock = sendFrame sock (serialise (Parcel PingHeader (Payload BSL.empty)))

readSock :: HasAriviNetworkInstance m => Conn.Connection -> HM.HashMap MessageId BSL.ByteString -> m ()
readSock connection fragmentsHM = do
  let sock = Conn.socket connection
  parcelOrFail <- liftIO $ getParcelWithTimeout sock 30000000
  case parcelOrFail of
    -- Possibly throw before shutting down
    Left (AriviDeserialiseException e) -> deleteConnectionFromHashMap (Conn.connectionId connection)
    Left AriviTimeoutException -> do
      liftIO $ sendPing (Conn.socket connection)
      -- Possibly throw before shutting down
      parcelOrFailAfterPing <- liftIO $ getParcelWithTimeout sock 60000000
      case parcelOrFailAfterPing of
        Left _  -> deleteConnectionFromHashMap (Conn.connectionId connection)
        Right parcel -> processParcel parcel connection fragmentsHM
    Right parcel -> processParcel parcel connection fragmentsHM


processParcel :: HasAriviNetworkInstance m => Parcel -> Conn.Connection -> HM.HashMap MessageId BSL.ByteString -> m ()
processParcel parcel connection fragmentsHM =
  case parcel of
    dataParcel@(Parcel DataHeader{} _) -> do
      res <- liftIO (try $ atomically $ reassembleFrames connection dataParcel fragmentsHM :: IO (Either SomeException (HM.HashMap MessageId BSL.ByteString)))
      case res of
        -- possibly throw again
        Left e -> traceShow e (return())>> deleteConnectionFromHashMap (Conn.connectionId connection)
        Right updatedHM -> do
          traceShow "asda" (return())
          -- msg <- liftIO $ atomically $ readTChan (p2pMessageTChan connection)
          -- traceShow msg (return())
          readSock connection updatedHM
    _ -> deleteConnectionFromHashMap(Conn.connectionId connection)



readHandshakeInitSock :: Socket -> IO Parcel
readHandshakeInitSock sock = do
  parcel <- getParcel sock
  either throwIO return parcel



-- | Read on the socket for a handshakeRespParcel and return it or throw appropriate AriviException
readHandshakeRespSock :: Socket -> SecretKey -> IO Parcel
readHandshakeRespSock sock sk = do
  parcelOrFail <- getParcelWithTimeout sock 30000000
  case parcelOrFail of
    Left (AriviDeserialiseException e) ->
      do
        sendFrame sock $ BSLC.pack (displayException e)
        throw $ AriviDeserialiseException e
    Left e -> throw e
    Right hsRespParcel ->
      case hsRespParcel of
        parcel@(Parcel (HandshakeRespHeader _ _ ) _ ) -> return parcel
        _ -> throw AriviWrongParcelException


deleteConnectionFromHashMap :: HasAriviNetworkInstance m
                            => Conn.ConnectionId
                            -> m ()
deleteConnectionFromHashMap connId = do
  ariviInstance <- getAriviNetworkInstance
  let tv = connectionMap ariviInstance
  liftIO $ atomically $ modifyTVar tv (HM.delete connId)
