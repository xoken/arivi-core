{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
    ( getIPAddress
    , getPortNumber
    , getTransportType
    , handleInboundConnection
    , readHandshakeRespSock
    , readSock
    , sendMessage
    , closeConnection
    ) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection       as Conn
import           Arivi.Network.Fragmenter
import           Arivi.Network.Handshake
import           Arivi.Network.Reassembler
import           Arivi.Network.StreamClient
import           Arivi.Network.Types
import           Arivi.Network.Utils            (getIPAddress, getPortNumber)
import           Arivi.Utils.Exception
import           Control.Concurrent             (MVar, newMVar, threadDelay)
import qualified Control.Concurrent.Async       as Async (race)
import           Control.Concurrent.STM         (atomically, newTChan)
import           Control.Concurrent.STM.TVar
import           Control.Exception              (try)
import           Control.Exception.Base
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.ByteString.Lazy.Char8     as BSLC
import           Data.HashMap.Strict            as HM
import           Data.Int
import           Data.IORef
import           Network.Socket                 (Socket (MkSocket),
                                                 SocketType (..), close,
                                                 getSocketName)
import qualified Network.Socket.ByteString.Lazy as N (recv)

handleInboundConnection ::
       (HasSecretKey m, HasLogging m)
    => Socket
    -> (ConnectionHandle -> m ())
    -> m ()
handleInboundConnection sock handler =
    $(withLoggingTH)
        (LogNetworkStatement "handleInboundConnection: ")
        LevelDebug $ do
        sk <- getSecretKey
        conn <-
            liftIO $ do
                socketName <- getSocketName sock
                ip <- getIPAddress socketName
                let portNum = getPortNumber socketName
                    tt = getTransportType sock
                    cId = Conn.makeConnectionId ip portNum tt
                egressNonce <- newTVarIO (2 :: SequenceNum)
                ingressNonce <- newTVarIO (2 :: SequenceNum)
          -- TODO: Need to change this to proper value
                aeadNonce <- newTVarIO (2 ^ 63 + 1 :: AeadNonce)
                p2pMsgTChan <- atomically newTChan
                writeLock <- newMVar 0
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
                        , Conn.aeadNonceCounter = aeadNonce
                        }
          -- getParcel, recipientHandshake and sendFrame might fail
          -- In any case, the thread just quits
                (serialisedParcel, updatedConn) <-
                    readHandshakeInitSock sock >>=
                    recipientHandshake sk connection
                sendFrame
                    (Conn.waitWrite updatedConn)
                    (Conn.socket updatedConn)
                    (createFrame serialisedParcel)
                return updatedConn
        fragmentsHM <- liftIO $ newIORef HM.empty
        handler
            ConnectionHandle
            { send = sendMessage conn
            , Arivi.Network.Types.recv = readSock conn fragmentsHM
            , Arivi.Network.Types.close = closeConnection (Conn.socket conn)
            }

-- | Given `Socket` retrieves `TransportType`
getTransportType :: Socket -> TransportType
getTransportType (MkSocket _ _ Stream _ _) = TCP
getTransportType _                         = UDP

-- | Converts length in byteString to Num
getFrameLength :: BSL.ByteString -> Int64
getFrameLength len = fromIntegral lenInt16
  where
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
                        (return . Left . AriviDeserialiseException)
                        (return . Right)
                        (deserialiseOrFail parcelCipher)

-- | Reads frame a given socket
getParcel :: Socket -> IO (Either AriviException Parcel)
getParcel sock = do
    lenbs <- recvAll sock 2
    parcelCipher <- recvAll sock $ getFrameLength lenbs
    either
        (return . Left . AriviDeserialiseException)
        (return . Right)
        (deserialiseOrFail parcelCipher)

-- | Create and send a ping message on the socket
sendPing :: MVar Int -> Socket -> IO ()
sendPing writeLock sock =
    let pingFrame =
            createFrame $ serialise (Parcel PingHeader (Payload BSL.empty))
    in sendFrame writeLock sock pingFrame

-- | Create and send a pong message on the socket
sendPong :: MVar Int -> Socket -> IO ()
sendPong writeLock sock =
    let pongFrame =
            createFrame $ serialise (Parcel PongHeader (Payload BSL.empty))
    in sendFrame writeLock sock pongFrame


sendMessage ::
       forall m. (MonadIO m, HasLogging m)
    => Conn.CompleteConnection
    -> BSLC.ByteString
    -> m ()
sendMessage conn msg =
    $(withLoggingTH) (LogNetworkStatement "Sending Message: ") LevelInfo $ do
        let sock = Conn.socket conn
            lock = Conn.waitWrite conn
        fragments <- liftIO $ processPayload (Payload msg) conn
        mapM_
            (\frame ->
                 liftIO (atomically frame >>= (try . sendFrame lock sock)) >>= \case
                     Left (_ :: SomeException) ->
                         closeConnection sock >> throw AriviSocketException
                     Right _ -> return ())
            fragments

closeConnection :: (HasLogging m) => Socket -> m ()
closeConnection sock = liftIO $ Network.Socket.close sock


readSock ::
       (HasLogging m)
    => Conn.CompleteConnection
    -> IORef (HM.HashMap MessageId BSL.ByteString)
    -> m BSL.ByteString
readSock connection fragmentsHM =
    $(withLoggingTH) (LogNetworkStatement "readSock: ") LevelDebug $ do
        let sock = Conn.socket connection
            writeLock = Conn.waitWrite connection
        parcelOrFail <- liftIO $ getParcelWithTimeout sock 30000000
        case parcelOrFail of
            Left (AriviDeserialiseException e) ->
                throw $ AriviDeserialiseException e
            Left AriviTimeoutException -> do
                liftIO $ sendPing writeLock sock
                parcelOrFailAfterPing <-
                    liftIO $ getParcelWithTimeout sock 60000000
                case parcelOrFailAfterPing of
                    Left e -> throw e
                    Right parcel ->
                        processParcel parcel connection fragmentsHM >>=
                          \case
                          Nothing -> readSock connection fragmentsHM
                          Just p2pMsg -> return p2pMsg
            Left e -> throw e
            Right parcel ->
                processParcel parcel connection fragmentsHM >>=
                \case
                Nothing -> readSock connection fragmentsHM
                Just p2pMsg -> return p2pMsg


processParcel ::
       (HasLogging m)
    => Parcel
    -> Conn.CompleteConnection
    -> IORef (HM.HashMap MessageId BSL.ByteString)
    -> m (Maybe BSL.ByteString)
processParcel parcel connection fragmentsHM =
    case parcel of
        dataParcel@(Parcel DataHeader {} _) -> do
            hm <- liftIO $ readIORef fragmentsHM
            let (hmUpdated, p2pMsg) = reassembleFrames connection dataParcel hm
            liftIO $ writeIORef fragmentsHM hmUpdated
            return p2pMsg
        pingParcel@(Parcel PingHeader {} _) -> do
            liftIO $ sendPong (Conn.waitWrite connection) (Conn.socket connection)
            return Nothing
        pongParcel@(Parcel PongHeader {} _) -> return Nothing
        _ -> throw AriviWrongParcelException

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
        Left (AriviDeserialiseException e) -> do
            sendFrame writeLock sock $ BSLC.pack (displayException e)
            throw $ AriviDeserialiseException e
        Left e -> throw e
        Right hsRespParcel ->
            case hsRespParcel of
                parcel@(Parcel (HandshakeRespHeader _ _) _) -> return parcel
                _ -> throw AriviWrongParcelException

-- Helper Functions
recvAll :: Socket -> Int64 -> IO BSL.ByteString
recvAll sock len = do
    msg <-
        mapIOException
            (\(_ :: SomeException) -> AriviSocketException)
            (N.recv sock len)
    if BSL.null msg
        then throw AriviSocketException
        else if BSL.length msg == len
                 then return msg
                 else BSL.append msg <$> recvAll sock (len - BSL.length msg)

