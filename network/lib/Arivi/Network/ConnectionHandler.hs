{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Arivi.Utils.Exception
import           Control.Concurrent              (threadDelay, MVar, newMVar)
import qualified Control.Concurrent.Async        as Async (async, race)
import qualified Control.Concurrent.Async.Lifted as LA (async)
import           Control.Concurrent.STM          (TChan, atomically, newTChan,
                                                  readTChan)
import           Control.Concurrent.STM.TVar
import           Control.Exception               (try, mapException)
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
import qualified Network.Socket.ByteString.Lazy  as N (recv)

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
handleInboundConnection mSocket = $(withLoggingTH) (LogNetworkStatement "handleInboundConnection: ") LevelDebug $  do
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
          egressNonce <- newTVarIO (2 :: SequenceNum)
          ingressNonce <- newTVarIO (2 :: SequenceNum)
          -- Need to change this to proper value
          aeadNonce <- newTVarIO (2^63+1 :: AeadNonce)
          p2pMsgTChan <- atomically newTChan
          writeLock <- newMVar 0
          let connection = Conn.mkIncompleteConnection' { Conn.connectionId = mConnectionId
                                                        , Conn.ipAddress = mIpAddress
                                                        , Conn.port = mPort
                                                        , Conn.transportType = mTransportType
                                                        , Conn.personalityType = RECIPIENT
                                                        , Conn.socket = mSocket
                                                        , Conn.waitWrite = writeLock
                                                        , Conn.p2pMessageTChan = p2pMsgTChan
                                                        , Conn.egressSeqNum = egressNonce
                                                        , Conn.ingressSeqNum = ingressNonce
                                                        , Conn.aeadNonceCounter = aeadNonce
                                                        }
          -- getParcel, recipientHandshake and sendFrame might fail
          -- In any case, the thread just quits
          parcel <- readHandshakeInitSock mSocket
          (serialisedParcel, updatedConn) <- recipientHandshake sk connection parcel
          sendFrame (Conn.waitWrite updatedConn) (Conn.socket updatedConn) (createFrame serialisedParcel)
          atomically $ modifyTVar tv (HM.insert mConnectionId updatedConn)
          return updatedConn
        LA.async $ readSock conn HM.empty
        return ()

-- | Given `SockAddr` retrieves `HostAddress`
getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet _ hostAddress) = hostAddress
getIPAddress (SockAddrInet6 _ _ (_,_,_,hA6) _) = hA6
getIPAddress _                            = error "getIPAddress: SockAddr is not of constructor SockAddrInet "

-- | Given `SockAddr` retrieves `PortNumber`
getPortNumber :: SockAddr -> PortNumber
getPortNumber (SockAddrInet portNumber _) = portNumber
getPortNumber (SockAddrInet6 portNumber _ _ _) = portNumber
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
getFrameLength :: BSL.ByteString -> Int64
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode len :: Int16

-- | Races between a timer and receive parcel, returns an either type
getParcelWithTimeout :: Socket -> Int -> IO (Either AriviException Parcel)
getParcelWithTimeout sock timeout = do
    winner <- Async.race (threadDelay timeout) (try $ recvAll sock 2)
    case winner of
      Left _ -> return $ Left AriviTimeoutException
      Right lenbsOrFail -> 
        case lenbsOrFail of  
          Left e -> return (Left e)
          Right lenbs -> do
            parcelCipher <- recvAll sock (getFrameLength lenbs) 
            either
              (return . Left . AriviDeserialiseException) (return . Right)
              (deserialiseOrFail parcelCipher)

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either AriviException Parcel)
getParcel sock = do
    lenbs <- recvAll sock 2
    parcelCipher <- recvAll sock $ getFrameLength lenbs
    either (return . Left . AriviDeserialiseException) (return . Right)
      (deserialiseOrFail parcelCipher)

-- | Create and send a ping message on the socket
sendPing :: MVar Int -> Socket -> IO()
sendPing writeLock sock =
  let pingFrame = createFrame $ serialise (Parcel PingHeader (Payload BSL.empty))
  in
  sendFrame writeLock sock pingFrame

-- | Create and send a pong message on the socket
sendPong :: MVar Int -> Socket -> IO()
sendPong writeLock sock =
  let pongFrame = createFrame $ serialise (Parcel PongHeader (Payload BSL.empty))
  in
  sendFrame writeLock sock pongFrame

readSock :: (HasAriviNetworkInstance m, HasLogging m) => Conn.CompleteConnection -> HM.HashMap MessageId BSL.ByteString -> m ()
readSock connection fragmentsHM = $(withLoggingTH) (LogNetworkStatement "readSock: ") LevelDebug $ do
  let sock      = Conn.socket connection
      writeLock = Conn.waitWrite connection
  parcelOrFail <- liftIO $ getParcelWithTimeout sock 30000000
  case parcelOrFail of
    -- Possibly throw before shutting down
    Left (AriviDeserialiseException e) -> deleteConnectionFromHashMap (Conn.connectionId connection)
    Left AriviTimeoutException -> do
      liftIO $ sendPing writeLock sock
      -- Possibly throw before shutting down
      parcelOrFailAfterPing <- liftIO $ getParcelWithTimeout sock 60000000
      case parcelOrFailAfterPing of
        Left _  -> deleteConnectionFromHashMap (Conn.connectionId connection)
        Right parcel -> processParcel parcel connection fragmentsHM >>=
                        readSock connection
    Left AriviSocketException -> deleteConnectionFromHashMap (Conn.connectionId connection)
    Right parcel -> processParcel parcel connection fragmentsHM >>=
                    readSock connection


processParcel :: (HasAriviNetworkInstance m, HasLogging m) => Parcel -> Conn.CompleteConnection -> HM.HashMap MessageId BSL.ByteString -> m (HM.HashMap MessageId BSL.ByteString)
processParcel parcel connection fragmentsHM =
  case parcel of
    dataParcel@(Parcel DataHeader{} _) -> do
      res <- liftIO (try $ atomically $ reassembleFrames connection dataParcel fragmentsHM :: IO (Either AriviException (HM.HashMap MessageId BSL.ByteString)))
      case res of
        -- possibly throw again
        Left e -> deleteConnectionFromHashMap (Conn.connectionId connection) >> throw e
        Right updatedHM -> return updatedHM
    pingParcel@(Parcel PingHeader{} _) -> do
      liftIO $ sendPong (Conn.waitWrite connection) (Conn.socket connection)
      return fragmentsHM
    pongParcel@(Parcel PongHeader{} _) -> return fragmentsHM
    _ -> deleteConnectionFromHashMap(Conn.connectionId connection) >> throw AriviWrongParcelException


-- | Read on the socket for handshakeInit parcel and return it or throw AriviException
readHandshakeInitSock :: Socket -> IO Parcel
readHandshakeInitSock sock = do
  parcel <- getParcel sock
  either throwIO return parcel

-- | Read on the socket for a handshakeRespParcel and return it or throw appropriate AriviException
readHandshakeRespSock :: MVar Int -> Socket -> SecretKey -> IO Parcel
readHandshakeRespSock writeLock sock sk = do
  parcelOrFail <- getParcelWithTimeout sock 30000000
  case parcelOrFail of
    Left (AriviDeserialiseException e) ->
      do
        sendFrame writeLock sock $ BSLC.pack (displayException e)
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
  hmap <- liftIO $ readTVarIO tv
  let connOrFail = HM.lookup connId hmap
  case connOrFail of
    Nothing -> return ()
    Just conn -> liftIO (close (Conn.socket conn)) >> liftIO (atomically $ modifyTVar tv (HM.delete connId))

-- Helper Functions
recvAll :: Socket
        -> Int64
        -> IO BSL.ByteString
recvAll sock len = do
  msg <- mapIOException (\(_::SomeException) -> AriviSocketException) (N.recv sock len)
  if BSL.null msg then
    throw AriviSocketException
  else
    if BSL.length msg == len
      then return msg
        else BSL.append msg <$> recvAll sock (len - BSL.length msg)

mapIOException f ioa = do
  aOrFail <- try ioa
  case aOrFail of
    Left (e :: SomeException) -> throw (f e)
    Right r -> return r        