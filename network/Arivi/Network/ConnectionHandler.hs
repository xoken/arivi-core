{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- {-# LANGUAGE LambdaCase        #-}
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
   , readSock
) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection        as Conn
import           Arivi.Network.Handshake
import           Arivi.Network.StreamClient
import           Arivi.Network.Types
import           Debug.Trace
-- import           Arivi.Network.Types             (DeserialiseFailure,
--                                                   Event (..), Header (..),
--                                                   Parcel (..),
--                                                   deserialiseOrFail)
import           Arivi.Network.Reassembler
import           Arivi.Network.Utils             (getIPAddress, getPortNumber)
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
import           Network.Socket                  (HostAddress, SockAddr (..),
                                                  Socket (..), SocketType (..),
                                                  accept, bind, connect,
                                                  getSocketName, inet_ntoa)
import qualified Network.Socket.ByteString       as Network (recv, recvFrom)

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
        liftIO $ traceShow "Inside handleInboundConnection" (return())
        sk <- getSecretKey
        ariviInstance <- getAriviNetworkInstance
        let tv = ariviNetworkConnectionMap ariviInstance
        let updatesTChan = ariviConnectionUpdatesTChan ariviInstance
        conn <- liftIO $ do
          socketName <- getSocketName mSocket
          mIpAddress <- getIPAddress socketName
          let mPort = getPortNumber socketName
          let mTransportType = getTransportType mSocket
          let mConnectionId = Conn.makeConnectionId mIpAddress mPort mTransportType
          egressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
          ingressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
          -- Need to change this to proper value
          aeadNonce <- liftIO (newTVarIO (2^63+1 :: AeadNonce))
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
                                           , Conn.egressSeqNum = egressNonce
                                           , Conn.ingressSeqNum = ingressNonce
                                           , Conn.aeadNonceCounter = aeadNonce
                                           }
          -- getParcel, recipientHandshake and sendFrame might fail
          -- In any case, the thread just quits
          liftIO $ traceShow "before readHandshakeInitSock" (return())

          parcel <- readHandshakeInitSock mSocket
          liftIO $ traceShow "after readHandshakeInitSock" (return())

          traceShow parcel (return())
          (serialisedParcel, updatedConn) <- recipientHandshake sk connection parcel
          sendFrame (Conn.socket updatedConn) (createFrame serialisedParcel
                                                           mTransportType)
          atomically $ modifyTVar tv (HM.insert mConnectionId updatedConn)
          return updatedConn
        LA.async $ readSock conn HM.empty
        return ()

-- handleUDPInboundConnection :: (HasAriviNetworkInstance m
--                               , HasSecretKey m
--                               , HasLogging m)
--                            => Socket
--                            -> BSLByteString
--                            -> m ()
-- handleUDPInboundConnection slefSocket message = do

--       sk <- getSecretKey

--       ariviInstance <- getAriviNetworkInstance
--       let tv = ariviNetworkConnectionMap ariviInstance
--       let updatesTChan = ariviConnectionUpdatesTChan ariviInstance

--       conn <- liftIO $ do
--         socketName <- getSocketName slefSocket
--         mIpAddress <- inet_ntoa $ getIPAddress socketName
--         let mPort = getPortNumber socketName
--         let mTransportType = getTransportType slefSocket
--         let mConnectionId = Conn.makeConnectionId mIpAddress mPort mTransportType

--         egressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
--         ingressNonce <- liftIO (newTVarIO (2 :: SequenceNum))

--         -- Need to change this to proper value

--         aeadNonce <- liftIO (newTVarIO (2^63+1 :: AeadNonce))
--         mReassemblyTChan <- atomically newTChan
--         p2pMsgTChan <- atomically newTChan

--         let connection = Conn.Connection { Conn.connectionId = mConnectionId
--                                          , Conn.ipAddress = mIpAddress
--                                          , Conn.port = mPort
--                                          , Conn.transportType = mTransportType
--                                          , Conn.personalityType = RECIPIENT
--                                          , Conn.socket = slefSocket
--                                          , Conn.reassemblyTChan = mReassemblyTChan
--                                          , Conn.p2pMessageTChan = p2pMsgTChan
--                                          , Conn.egressSeqNum = egressNonce
--                                          , Conn.ingressSeqNum = ingressNonce
--                                          , Conn.aeadNonceCounter = aeadNonce
--                                          }
--         -- getParcel, recipientHandshake and sendFrame might fail
--         -- In any case, the thread just quits
--         parcel <- readHandshakeInitSock slefSocket

--         (serialisedParcel, updatedConn) <- recipientHandshake sk connection parcel

--         sendFrame (Conn.socket updatedConn) (createFrame serialisedParcel
--                                                          mTransportType)

--         atomically $ modifyTVar tv (HM.insert mConnectionId updatedConn)

--         return updatedConn
--       LA.async $ readSock conn HM.empty
--       return ()



-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ Stream _ _)   = TCP
getTransportType (MkSocket _ _ Datagram _ _) = UDP

-- | Server Thread that spawns new thread to
-- | listen to client and put it to inboundTChan
acceptIncomingSocket :: ( HasAriviNetworkInstance m
                        , HasSecretKey m
                        , HasLogging m)
                     => Socket
                     -> m a
acceptIncomingSocket selfSocket = do
  sk <- getSecretKey
  case getTransportType selfSocket of
      TCP -> forever $ do
                  (mSocket, peer) <- liftIO $ accept selfSocket
                  liftIO $ putStrLn $ "Connection from " ++ show peer
                  eventTChan <- liftIO $ atomically newTChan
                  LA.async (handleInboundConnection mSocket) --or use forkIO
      UDP -> -- forever $ do
            do
             -- (message,sockAddra) <- Network.recvFrom selfSocket 4096
             LA.async (handleInboundConnection selfSocket)   --or use forkIO
    -- threadDelay 1000000000000
             return undefined



-- | Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Races between a timer and receive parcel, returns an either type
getParcelWithTimeout :: Socket -> Int -> IO (Either AriviException Parcel)
getParcelWithTimeout sock timeout = do
      winner <- Async.race (threadDelay timeout) (Network.recv sock 2)
      -- traceShow "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" (return())
      -- traceShow winner (return())
      -- traceShow "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" (return())
      case winner of
        Left _ -> return $ Left AriviTimeoutException
        Right lenbs ->
          do
            parcelCipher <- Network.recv sock $ getFrameLength lenbs
            either
              (return . Left . AriviDeserialiseException) (return . Right)
              (deserialiseOrFail (BSL.fromStrict parcelCipher))

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either AriviException Parcel)
getParcel sock = do
    lenbs <- Network.recv sock 2
    parcelCipher <- Network.recv sock $ getFrameLength lenbs
    either (return . Left . AriviDeserialiseException) (return . Right)
      (deserialiseOrFail (BSL.fromStrict parcelCipher))

-- -- | Reads frame a given socket
-- getParcelUDP :: Socket -> IO (Either AriviException (Parcel))
-- getParcelUDP sock = do
--      -- parcelCipher <- Network.recv sock 4096
--      (parcelCipher,peerAddr) <- Network.recvFrom sock 4096
--      -- connect sock peerAddr
--      either (return . Left . AriviDeserialiseException) (return . Right)
--       (deserialiseOrFail (BSL.fromStrict parcelCipher))

-- -- | Create and send a ping message on the socket
sendPing :: Socket -> TransportType -> IO()
sendPing sock mTransportType = do
  let pingFrame = createFrame (serialise (Parcel PingHeader (Payload BSL.empty)))
                               mTransportType
  sendFrame sock pingFrame

-- | Create and send a pong message on the socket
sendPong :: Socket -> TransportType -> IO()
sendPong sock mTransportType = do
  let pongFrame = createFrame  (serialise (Parcel PongHeader (Payload BSL.empty)))
                                mTransportType
  sendFrame sock pongFrame

readSock :: HasAriviNetworkInstance m => Conn.Connection -> HM.HashMap MessageId BSL.ByteString -> m ()
readSock connection fragmentsHM = do
  let sock = Conn.socket connection
  parcelOrFail <- liftIO $ getParcelWithTimeout sock 3000000
  case parcelOrFail of
    -- Possibly throw before shutting down
    Left (AriviDeserialiseException e) -> deleteConnectionFromHashMap (Conn.connectionId connection)
    Left AriviTimeoutException -> do
      liftIO $ sendPing sock (transportType connection)
      -- Possibly throw before shutting down
      parcelOrFailAfterPing <- liftIO $ getParcelWithTimeout sock 6000000
      case parcelOrFailAfterPing of
        Left _  -> deleteConnectionFromHashMap (Conn.connectionId connection)
        Right parcel -> processParcel parcel connection fragmentsHM
    Right parcel -> processParcel parcel connection fragmentsHM


processParcel :: HasAriviNetworkInstance m => Parcel -> Conn.Connection -> HM.HashMap MessageId BSL.ByteString -> m ()
processParcel parcel connection fragmentsHM =
  -- traceShow parcel (return()) >>
  case parcel of
    dataParcel@(Parcel DataHeader{} _) -> do
      res <- liftIO (try $ atomically $ reassembleFrames connection dataParcel fragmentsHM :: IO (Either AriviException (HM.HashMap MessageId BSL.ByteString)))
      case res of
        -- possibly throw again
        Left e -> deleteConnectionFromHashMap (Conn.connectionId connection)
        Right updatedHM ->
          readSock connection updatedHM
    pingParcel@(Parcel PingHeader{} _) -> do
      liftIO $ sendPong (Conn.socket connection) (transportType connection)
      readSock connection fragmentsHM
    pongParcel@(Parcel PongHeader{} _) -> readSock connection fragmentsHM
    _ -> deleteConnectionFromHashMap(Conn.connectionId connection)


-- | Read on the socket for handshakeInit parcel and return it or throw AriviException
readHandshakeInitSock :: Socket -> IO Parcel
readHandshakeInitSock sock = do
      traceShow "inside readHandshakeInitSock TCP" (return())
      parcel <- getParcel sock
      either throwIO return parcel


-- | Read on the socket for a handshakeRespParcel and return it or throw appropriate AriviException
readHandshakeRespSock :: Socket -> SecretKey -> IO Parcel
readHandshakeRespSock sock sk = do
  -- traceShow "" (return())
  -- traceShow "" (return())
  -- traceShow "" (return())
  -- traceShow "before parcelOrFail inside readHandshakeRespSock"  (return())
  parcelOrFail <- getParcelWithTimeout sock 30000000
  -- traceShow (parcelOrFail)  (return())
  -- traceShow "after parcelOrFail inside readHandshakeRespSock" (return())
  -- traceShow "" (return())
  -- traceShow "" (return())
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
        -- parcel@_ -> return parcel


deleteConnectionFromHashMap :: HasAriviNetworkInstance m
                            => Conn.ConnectionId
                            -> m ()
deleteConnectionFromHashMap connId = do
  ariviInstance <- getAriviNetworkInstance
  let tv = connectionMap ariviInstance
  liftIO $ atomically $ modifyTVar tv (HM.delete connId)
