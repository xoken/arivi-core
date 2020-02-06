{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

--
-- This module provides useful functions for managing dispatch of frames in
-- Arivi communication
module Arivi.Network.ConnectionHandler
    ( establishSecureConnection
    , readHandshakeInitSock
    , readHandshakeRespSock
    , readUdpHandshakeRespSock
    , readTcpSock
    , readUdpSock
    , sendTcpMessage
    , sendUdpMessage
    , closeConnection
    ) where

import Arivi.Network.Connection as Conn
import Arivi.Network.Exception
import Arivi.Network.Fragmenter
import Arivi.Network.Handshake
import Arivi.Network.Reassembler
import Arivi.Network.Replay (isReplayAttack, noOfPendings)
import Arivi.Network.StreamClient
import Arivi.Network.Types hiding (ip)
import Arivi.Network.Utils (getIPAddress, getPortNumber)
import Arivi.Utils.Logging
import Control.Concurrent (MVar, newMVar, threadDelay)
import qualified Control.Concurrent.Async as Async (race)
import Control.Concurrent.STM (atomically, newTChan)
import Control.Concurrent.STM.TVar
import Control.Exception (try)
import Control.Exception.Base
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.HashMap.Strict as HM
import Data.IORef
import Data.Int
import Network.Socket hiding (send)
import qualified Network.Socket.ByteString.Lazy as N (recv)

establishSecureConnection ::
       SecretKey -> Socket -> (BSL.ByteString -> BSL.ByteString) -> Parcel -> IO CompleteConnection
establishSecureConnection sk sock framer hsInitParcel = do
    socketName <- getPeerName sock
    ip <- getIPAddress socketName
    portNum <- getPortNumber socketName
    let tt = getTransportType sock
    let cId = Conn.makeConnectionId ip portNum tt
    egressNonce <- newTVarIO (2 :: SequenceNum)
    ingressNonce <- newTVarIO (2 :: SequenceNum)
          -- TODO: Need to change this to proper value
    initNonce <- newTVarIO (2 ^ 63 + 1 :: AeadNonce)
    p2pMsgTChan <- atomically newTChan
    writeLock <- newMVar 0
    mPendingList <- newTVarIO [(0, (2 :: Integer) ^ (32 :: Integer))]
    let connection =
            Conn.mkIncompleteConnection'
                { Conn.connectionId = cId
                , Conn.ipAddress = ip
                , Conn.port = portNum
                , Conn.transportType = tt
                , Conn.personalityType = RECIPIENT
                , Conn.socket = sock
                , Conn.waitWrite = writeLock
                , Conn.p2pMessageTChan = p2pMsgTChan
                , Conn.egressSeqNum = egressNonce
                , Conn.ingressSeqNum = ingressNonce
                , Conn.aeadNonceCounter = initNonce
                , Conn.pendingList = mPendingList
                }
          -- getParcel, recipientHandshake and sendFrame might fail
          -- In any case, the thread just quits
    (serialisedParcel, updatedConn) <- recipientHandshake sk connection hsInitParcel
    sendFrame (Conn.waitWrite updatedConn) (Conn.socket updatedConn) (framer serialisedParcel)
    return updatedConn

-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ Stream _ _) = TCP
getTransportType _ = UDP

-- | Converts length in byteString to Num
getFrameLength :: BSL.ByteString -> Int64
getFrameLength len = fromIntegral lenInt16
  where
    lenInt16 = decode len :: Int16

-- | Races between a timer and receive parcel, returns an either type
getParcelWithTimeout :: Socket -> Int -> IO (Either AriviNetworkException Parcel)
getParcelWithTimeout sock microseconds = do
    winner <- Async.race (threadDelay microseconds) (try $ recvAll sock 2)
    case winner of
        Left _ -> return $ Left NetworkTimeoutException
        Right lenbsOrFail ->
            case lenbsOrFail of
                Left e -> return (Left e)
                Right lenbs -> do
                    parcelCipher <- recvAll sock (getFrameLength lenbs)
                    either
                        (return . Left . NetworkDeserialiseException)
                        (return . Right)
                        (deserialiseOrFail parcelCipher)

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either AriviNetworkException Parcel)
getParcel sock = do
    lenbs <- recvAll sock 2
    parcelCipher <- recvAll sock $ getFrameLength lenbs
    either (return . Left . NetworkDeserialiseException) (return . Right) (deserialiseOrFail parcelCipher)

-- | Create and send a ping message on the socket
sendPing :: MVar Int -> Socket -> Framer -> IO ()
sendPing writeLock sock framer =
    let pingFrame = framer $ serialise (Parcel PingHeader (Payload BSL.empty))
     in sendFrame writeLock sock pingFrame

-- | Create and send a pong message on the socket
sendPong :: MVar Int -> Socket -> Framer -> IO ()
sendPong writeLock sock framer =
    let pongFrame = framer $ serialise (Parcel PongHeader (Payload BSL.empty))
     in sendFrame writeLock sock pongFrame

sendTcpMessage ::
       forall m. (MonadIO m, HasLogging m)
    => Conn.CompleteConnection
    -> BSLC.ByteString
    -> m ()
sendTcpMessage conn msg = do
    let sock = Conn.socket conn
        lock = Conn.waitWrite conn
    fragments <- liftIO $ processPayload 1024 (Payload msg) conn
    mapM_
        (\frame ->
             liftIO (atomically frame >>= (try . sendFrame lock sock)) >>= \case
                 Left (e :: SomeException) ->
                     liftIO (print "SendTcpMessage" >> print e) >> closeConnection sock >> throw NetworkSocketException
                 Right _ -> return ())
        fragments

sendUdpMessage ::
       forall m. (MonadIO m, HasLogging m)
    => Conn.CompleteConnection
    -> BSLC.ByteString
    -> m ()
sendUdpMessage conn msg = do
    let sock = Conn.socket conn
        lock = Conn.waitWrite conn
    fragments <- liftIO $ processPayload 4096 (Payload msg) conn
    mapM_
        (\frame ->
             liftIO (atomically frame >>= (try . sendFrame lock sock)) >>= \case
                 Left (e :: SomeException) -> liftIO (print e) >> closeConnection sock >> throw e
                 Right _ -> return ())
        fragments

closeConnection :: (HasLogging m) => Socket -> m ()
closeConnection sock = liftIO $ Network.Socket.close sock

readTcpSock ::
       (HasLogging m) => Conn.CompleteConnection -> IORef (HM.HashMap MessageId BSL.ByteString) -> m BSL.ByteString
readTcpSock connection fragmentsHM = do
    let sock = Conn.socket connection
        writeLock = Conn.waitWrite connection
    parcelOrFail <- liftIO $ getParcelWithTimeout sock 30000000
    case parcelOrFail of
        Left (NetworkDeserialiseException e) -> throw $ NetworkDeserialiseException e
        Left NetworkTimeoutException -> do
            liftIO $ sendPing writeLock sock createFrame
            parcelOrFailAfterPing <- liftIO $ getParcelWithTimeout sock 60000000
            case parcelOrFailAfterPing of
                Left e -> throw e
                Right parcel ->
                    processParcel parcel connection fragmentsHM >>= \case
                        Nothing -> readTcpSock connection fragmentsHM
                        Just p2pMsg -> return p2pMsg
        Left e -> throw e
        Right parcel ->
            processParcel parcel connection fragmentsHM >>= \case
                Nothing -> readTcpSock connection fragmentsHM
                Just p2pMsg -> return p2pMsg

processParcel ::
       (HasLogging m)
    => Parcel
    -> Conn.CompleteConnection
    -> IORef (HM.HashMap MessageId BSL.ByteString)
    -> m (Maybe BSL.ByteString)
processParcel parcel connection fragmentsHM =
    case parcel of
        Parcel (DataHeader _ _ _ _ replayNonce _) _ -> do
            hm <- liftIO $ readIORef fragmentsHM
            let (updatedHM, p2pMsg) = reassembleFrames connection parcel hm
            liftIO $ writeIORef fragmentsHM updatedHM
            -- let crtNonce = nonce p2pMsg
            mPendingList <- liftIO $ readTVarIO $ Conn.pendingList connection
            (isReplay, uPendingList) <- isReplayAttack (fromIntegral replayNonce) mPendingList
            pendingNos <- noOfPendings uPendingList
            if isReplay || (pendingNos > 1000)
                then throw ReplayAttackException
                else do
                    liftIO $ atomically $ writeTVar (Conn.pendingList connection) uPendingList
                    return p2pMsg
        Parcel PingHeader {} _ -> do
            liftIO $ sendPong (Conn.waitWrite connection) (Conn.socket connection) createFrame
            return Nothing
        Parcel PongHeader {} _ -> return Nothing
        _ -> throw NetworkWrongParcelException

getDatagramWithTimeout :: Socket -> Int -> IO (Either AriviNetworkException Parcel)
getDatagramWithTimeout sock microseconds = do
    datagramOrNothing <-
        Async.race
            (threadDelay microseconds)
            (try $ mapIOException (\(_ :: SomeException) -> NetworkSocketException) (N.recv sock 5100))
    case datagramOrNothing of
        Left _ -> return $ Left NetworkTimeoutException
        Right datagramEither ->
            case datagramEither of
                Left e -> return (Left e)
                Right datagram -> return $ first NetworkDeserialiseException $ deserialiseOrFail datagram

readUdpSock :: (HasLogging m) => Conn.CompleteConnection -> m BSL.ByteString
readUdpSock connection = do
    let sock = Conn.socket connection
        writeLock = Conn.waitWrite connection
    parcelOrFail <- liftIO $ getDatagramWithTimeout sock 30000000
    case parcelOrFail of
        Left (NetworkDeserialiseException e) -> throw $ NetworkDeserialiseException e
        Left NetworkTimeoutException -> do
            liftIO $ sendPing writeLock sock id
            parcelOrFailAfterPing <- liftIO $ getDatagramWithTimeout sock 60000000
            case parcelOrFailAfterPing of
                Left e -> throw e
                Right parcel ->
                    processDatagram connection parcel >>= \case
                        Nothing -> readUdpSock connection
                        Just p2pMsg -> return p2pMsg
        Left e -> throw e
        Right parcel ->
            processDatagram connection parcel >>= \case
                Nothing -> readUdpSock connection
                Just p2pMsg -> return p2pMsg

processDatagram :: (HasLogging m) => Conn.CompleteConnection -> Parcel -> m (Maybe BSL.ByteString)
processDatagram connection parcel =
    case parcel of
        Parcel DataHeader {} _ -> return . Just $ decryptPayload connection parcel
        Parcel PingHeader {} _ -> do
            liftIO $ sendPong (Conn.waitWrite connection) (Conn.socket connection) id
            return Nothing
        Parcel PongHeader {} _ -> return Nothing
        _ -> throw NetworkWrongParcelException

-- | Read on the socket for handshakeInit parcel and return it or throw AriviException
readHandshakeInitSock :: Socket -> IO Parcel
readHandshakeInitSock sock = do
    parcel <- getParcel sock
    either throwIO return parcel

-- | Read on the socket for a handshakeRespParcel and return it or throw appropriate AriviException
readHandshakeRespSock :: MVar Int -> Socket -> IO Parcel
readHandshakeRespSock writeLock sock = do
    parcelOrFail <- getParcelWithTimeout sock 30000000
    case parcelOrFail of
        Left (NetworkDeserialiseException e) -> do
            sendFrame writeLock sock $ BSLC.pack (displayException e)
            throw $ NetworkDeserialiseException e
        Left e -> throw e
        Right hsRespParcel ->
            case hsRespParcel of
                parcel@(Parcel (HandshakeRespHeader _ _) _) -> return parcel
                _ -> throw NetworkWrongParcelException

-- | Read on the socket for a handshakeRespParcel and return it or throw appropriate AriviException
readUdpHandshakeRespSock :: MVar Int -> Socket -> IO Parcel
readUdpHandshakeRespSock writeLock sock = do
    parcelOrFail <- getDatagramWithTimeout sock 30000000
    case parcelOrFail of
        Left (NetworkDeserialiseException e) -> do
            sendFrame writeLock sock $ BSLC.pack (displayException e)
            throw $ NetworkDeserialiseException e
        Left e -> throw e
        Right hsRespParcel ->
            case hsRespParcel of
                Parcel (HandshakeRespHeader _ _) _ -> return hsRespParcel
                _ -> throw NetworkWrongParcelException

-- Helper Functions
recvAll :: Socket -> Int64 -> IO BSL.ByteString
recvAll sock len = do
    msg <- mapIOException (\(_ :: SomeException) -> NetworkSocketException) (N.recv sock len)
    if BSL.null msg
        then throw NetworkSocketException
        else if BSL.length msg == len
                 then return msg
                 else BSL.append msg <$> recvAll sock (len - BSL.length msg)

type Framer = BSL.ByteString -> BSL.ByteString
