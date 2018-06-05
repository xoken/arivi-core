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
import           Arivi.Utils.Exception
import           Control.Concurrent              (threadDelay)
import qualified Control.Concurrent.Async        as Async (async, race)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM          (TChan, atomically, newTChan)
import           Control.Concurrent.STM.TVar
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
        liftIO $ do
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
          traceShow parcel (return ())
          (serialisedParcel, updatedConn) <- recipientHandshake sk connection parcel
          sendFrame (Conn.socket updatedConn) (createFrame serialisedParcel)
          atomically $ modifyTVar tv (HM.insert mConnectionId updatedConn)
          Async.async (readSock mSocket)
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
        -- threadDelay 5000000
        -- sendFrame mSocket (createFrame "msg")
        async (handleInboundConnection mSocket) --or use forkIO



-- | Converts length in byteString to Num
getFrameLength :: Num b => BS.ByteString -> b
getFrameLength len = fromIntegral lenInt16 where
                     lenInt16 = decode lenbs :: Int16
                     lenbs = BSL.fromStrict len

-- | Reads frame a given socket
getParcelWithTimeout :: Socket -> Int -> IO (Either AriviException Parcel)
getParcelWithTimeout sock timeout = do
    winner <- race (threadDelay timeout) (N.recv sock 2)
    case winner of
      Left _ -> return $ Left AriviTimeoutException
      Right lenbs ->
        do
          traceShow lenbs (return ())
          parcelCipher <- N.recv sock $ getFrameLength lenbs
          traceShow parcelCipher (return ())
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

readSock :: Socket -> IO ()
readSock sock =
  traceShow "here " (return ())>>
  getParcelWithTimeout sock 30000000 >>=
    \case
    Left e -> traceShow e (return ()) >> readSock sock
    Right r -> traceShow r (return ()) >> readSock sock
-- readSock sock eventTChan sk = forever $
--         getParcel sock >>=
--         either (sendFrame sock . BSLC.pack . displayException)
--                (\case
--                    e@(Parcel (HandshakeInitHeader _ _) _) -> do
--                      traceShow e (return ())
--                      atomically $ writeTChan eventTChan (KeyExchangeInitEvent e sk)
--                    e@(Parcel (HandshakeRespHeader _ _) _) -> do
--                     traceShow e (return ())
--                     atomically $ writeTChan eventTChan (KeyExchangeRespEvent e)
--                    e@(Parcel DataHeader {} _)    -> do
--                      traceShow e (return ())
--                      atomically $ writeTChan eventTChan (ReceiveDataEvent e)
--                    e                                      -> do
--                      traceShow e (return ())
--                      sendFrame sock "O traveller! Don't wander into uncharted terriories!"
--                )


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
