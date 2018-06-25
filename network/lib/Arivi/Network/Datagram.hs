{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Arivi.Network.Datagram
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication for Datagrams
module Arivi.Network.Datagram
    ( createUDPFrame
    , getUDPConnectionId
    , makeSocket
    , doEncryptedHandshakeForUDP
    , runUDPServerForever
    , sendUDPFrame
    , readFromUDPSocketForever
    ) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305   (getCipherTextAuthPair)
import           Arivi.Crypto.Utils.PublicKey.Utils   (decryptMsg)
import           Arivi.Env                            (HasAriviNetworkInstance,
                                                       HasSecretKey,
                                                       ariviNetworkConnectionMap,
                                                       ariviNetworkDatagramMap,
                                                       getAriviNetworkInstance,
                                                       getSecretKey)
import           Arivi.Logging                        (HasLogging)

import qualified Arivi.Network.Connection             as Conn
import           Arivi.Network.Handshake              (initiatorHandshake,
                                                       receiveHandshakeResponse,
                                                       recipientHandshake)
import           Arivi.Network.StreamClient           (sendFrame)
import           Arivi.Network.Types                  (AeadNonce, ConnectionId,
                                                       Header (..), Parcel (..),
                                                       Payload (..),
                                                       PersonalityType (..),
                                                       SequenceNum,
                                                       TransportType (..),
                                                       deserialiseOrFail,
                                                       serialise)
import qualified Arivi.Network.Utils                  as Utils (getIPAddress,
                                                                getPortNumber)
import           Arivi.Utils.Exception                (AriviException (..))
import           Control.Concurrent.Async.Lifted      (async)
import           Control.Concurrent.MVar              (MVar, putMVar, takeMVar)
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TChan         (TChan, newTChan,
                                                       readTChan, writeTChan)
import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVarIO, readTVarIO,
                                                       writeTVar)
import           Control.Exception                    (throw)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Control
import           Crypto.PubKey.Ed25519                (SecretKey)
import qualified Data.ByteString.Char8                as Char8 (ByteString,
                                                                pack)
import qualified Data.ByteString.Lazy                 as Lazy (ByteString,
                                                               concat,
                                                               fromStrict, pack,
                                                               toStrict)
import           Data.HashMap.Strict                  as StrictHashMap
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString            as Network (recvFrom,
                                                                  sendTo)
import qualified Network.Socket.ByteString.Lazy       as Network (sendAll)

import           Control.Monad.Catch                  (displayException)

import           Control.Concurrent.Async.Lifted.Safe (Forall, Pure)

makeSocket :: HostName -> PortNumber -> SocketType -> IO Socket
makeSocket ipAddress portNumber socketType = do
    let hint =
            defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = socketType}
    selfAddr:_ <-
        getAddrInfo (Just hint) (Just ipAddress) (Just (show portNumber))
    selfSocket <-
        socket
            (addrFamily selfAddr)
            (addrSocketType selfAddr)
            (addrProtocol selfAddr)
    bind selfSocket (addrAddress selfAddr)
    return selfSocket

-- runUDPServerForever sock = forever
--     $ do
--     (receivedMessage,peerSockAddr) <- Network.recvFrom sock 4096
--     let cid = sockAddrToConnectionId
runUDPServerForever ::
       ( MonadIO m
       , MonadBaseControl IO m
       , HasAriviNetworkInstance m
       , HasSecretKey m
       , HasLogging m
       , Forall (Pure m)
       )
    => Socket
    -> m ()
runUDPServerForever mSocket = do
    (receivedMessage, peerSockAddr) <- liftIO $ Network.recvFrom mSocket 4096
    ariviNetworkInstance <- getAriviNetworkInstance
    let hashMapTVar = ariviNetworkDatagramMap ariviNetworkInstance
    traceShow receivedMessage (return ())
    traceShow peerSockAddr (return ())
    cid <- liftIO $ getUDPConnectionId peerSockAddr
    hm <- liftIO $ readTVarIO hashMapTVar
    eitherExeptionParcel <- deserialiseParcel receivedMessage
    case eitherExeptionParcel of
        Left deserialiseException -> throw deserialiseException
        Right parcel -> do
            traceShow parcel (return ())
            traceShow "peerSockAddr" (return ())
            case StrictHashMap.lookup cid hm of
                Just hInboundDatagramTChan -> do
                    _ <-
                        liftIO $
                        atomically $ writeTChan hInboundDatagramTChan parcel
                    runUDPServerForever mSocket
                Nothing -> do
                    let pType = getPersonalityType parcel
                    _ <-
                        async
                            (processNewConnection
                                 cid
                                 hashMapTVar
                                 peerSockAddr
                                 mSocket
                                 parcel
                                 pType)
                    runUDPServerForever mSocket

processNewConnection ::
       ( MonadIO m
       , MonadBaseControl IO m
       , HasAriviNetworkInstance m
       , HasSecretKey m
       , HasLogging m
       , Forall (Pure m)
       )
    => ConnectionId
    -> TVar (HashMap ConnectionId (TChan Parcel))
    -> SockAddr
    -> Socket
    -> Parcel
    -> PersonalityType
    -> m ()
processNewConnection cid hashMapTVar peerSockAddr mSocket parcel pType = do
    mIpAddress <- liftIO $ Utils.getIPAddress peerSockAddr
    let mPort = Utils.getPortNumber peerSockAddr
    let mTransportType = UDP
    egressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
    ingressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
            -- Need to change this to proper value
    mAEADNonce <- liftIO (newTVarIO (2 ^ 63 + 1 :: AeadNonce))
    mInboundDatagramTChan <- liftIO $ atomically newTChan
    _ <- liftIO $ atomically $ writeTChan mInboundDatagramTChan parcel
    p2pMsgTChan <- liftIO $ atomically newTChan
    hsCompleteTVar <- liftIO $ newTVarIO Conn.HandshakeNotStarted
    let connection =
            Conn.mkIncompleteConnection'
            { Conn.connectionId = cid
            , Conn.ipAddress = mIpAddress
            , Conn.port = mPort
            , Conn.transportType = mTransportType
            , Conn.personalityType = pType
            , Conn.socket = mSocket
            , Conn.inboundDatagramTChan = mInboundDatagramTChan
            , Conn.p2pMessageTChan = p2pMsgTChan
            , Conn.egressSeqNum = egressNonce
            , Conn.ingressSeqNum = ingressNonce
            , Conn.aeadNonceCounter = mAEADNonce
            , Conn.handshakeComplete = hsCompleteTVar
            , Conn.remoteSockAddr = peerSockAddr
            }
    liftIO $
        atomically $
        modifyTVar hashMapTVar (StrictHashMap.insert cid mInboundDatagramTChan)
    -- sk <- getSecretKey
    let pType = getPersonalityType parcel
    traceShow pType (return ())
    newConnection <- doEncryptedHandshakeForUDP connection pType
    handleInboundDatagrams newConnection

-- handleInboundDatagrams newConnection = undefined
handleInboundDatagrams ::
       ( MonadIO m
       , MonadBaseControl IO m
       , HasAriviNetworkInstance m
       , HasSecretKey m
       , HasLogging m
       , Forall (Pure m)
       )
    => Conn.CompleteConnection
    -> m ()
handleInboundDatagrams connection = do
    handshakeStatus <- liftIO $ readTVarIO (Conn.handshakeComplete connection)
    parcel <-
        liftIO $ atomically $ readTChan (Conn.inboundDatagramTChan connection)
    traceShow parcel (return ())
    -- if handshakeStatus /= Conn.HandshakeDone
    --       -- doHandshake
    --     then do
    --         sk <- getSecretKey
    --         ariviNetworkInstance <- getAriviNetworkInstance
    --         let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance
    --         (serialisedParcel, updatedConn) <-
    --             liftIO $ recipientHandshake sk connection parcel
    --         traceShow "before sendUDPFrame HandshakeDone" (return ())
    --         _ <-
    --             liftIO $
    --             sendUDPFrame
    --                 (Conn.socket updatedConn)
    --                 (Conn.remoteSockAddr updatedConn)
    --                 (createUDPFrame serialisedParcel)
    --         traceShow "after sendUDPFrame HandshakeDone" (return ())
    --         let cid = Conn.connectionId updatedConn
    --         liftIO $
    --             atomically $
    --             writeTVar
    --                 (Conn.handshakeComplete updatedConn)
    --                 Conn.HandshakeDone
    --         liftIO $
    --             atomically $
    --             modifyTVar hashMapTVar (StrictHashMap.insert cid updatedConn)
    --         handleInboundDatagrams updatedConn
    --     else do
    let (cipherText, authenticationTag) =
            getCipherTextAuthPair
                (Lazy.toStrict (getPayload (encryptedPayload parcel)))
    let parcelHeader = Lazy.toStrict $ serialise (header parcel)
    let fragmentAead = aeadNonce (header parcel)
    let ssk = Conn.sharedSecret connection
    let payloadMessage =
            Lazy.fromStrict $
            decryptMsg
                fragmentAead
                ssk
                parcelHeader
                authenticationTag
                cipherText
    _ <-
        liftIO $
        atomically $ writeTChan (Conn.p2pMessageTChan connection) payloadMessage
    handleInboundDatagrams connection
    -- if (isNewConnection)
    --     then
    --         new hsBufferTChan
    --         insert into hsBufferTChan
    --         connection = makeConnection
    --         insert into HashMap
    --         handleUDPInboundConnection
    --     else do
    --         cid <- getUDPConnectionId peerSockAddr
    --         hsBufferTChan = getConnection nobejet using peerSockAddr
    --         insert into hsBufferTChan

readFromUDPSocketForever :: (MonadIO m, MonadBaseControl IO m, HasAriviNetworkInstance m) => Socket -> m ()
readFromUDPSocketForever mSocket = do
    traceShow "inside readFromUDPSocketForever" (return ())
    (receivedMessage, peerSockAddr) <- liftIO $ Network.recvFrom mSocket 4096
    traceShow "after readFromUDPSocketForever" (return ())
    ariviNetworkInstance <- getAriviNetworkInstance
    let hashMapTVar = ariviNetworkDatagramMap ariviNetworkInstance
    cid <- liftIO $ getUDPConnectionId peerSockAddr
    hmDatagram <- liftIO $ readTVarIO hashMapTVar
    eitherExeptionParcel <- deserialiseParcel receivedMessage
    case eitherExeptionParcel of
        Left deserialiseException -> throw deserialiseException
        Right parcel -> do
            traceShow parcel (return ())
            case StrictHashMap.lookup cid hmDatagram of
                Just mInboundDatagramTChan -> do
                    _ <-
                        liftIO $
                        atomically $ writeTChan mInboundDatagramTChan parcel
                    -- let newConnection =
                    --         conn {Conn.remoteSockAddr = peerSockAddr}
                    liftIO $
                        atomically $
                        modifyTVar
                            hashMapTVar
                            (StrictHashMap.insert cid mInboundDatagramTChan)
                    readFromUDPSocketForever mSocket
                Nothing -> throw AriviWrongParcelException

getUDPConnectionId :: SockAddr -> IO ConnectionId
getUDPConnectionId peerSockAddr = do
    ipAddress <- Utils.getIPAddress peerSockAddr
    let portNumber = show (Utils.getPortNumber peerSockAddr)
    return $ Char8.pack $ ipAddress ++ "|" ++ portNumber ++ "|" ++ show UDP

-- checkIsNewConnection :: (HasAriviNetworkInstance m)
--                 => SockAddr
--                 -> m Bool
-- checkIsNewConnection peerSockAddr = do
--     ariviNetworkInstance <- getAriviNetworkInstance
--     let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance
--     cid <- liftIO $ getUDPConnectionId peerSockAddr
--     hm <- liftIO $ readTVarIO hashMapTVar
--     case (StrictHashMap.lookup cid hm) of
--         Just _  -> return True
--         Nothing -> return False
deserialiseParcel ::
       (Monad m) => Char8.ByteString -> m (Either AriviException Parcel)
deserialiseParcel parcelCipher =
    either
        (return . Left . AriviDeserialiseException)
        (return . Right)
        (deserialiseOrFail (Lazy.fromStrict parcelCipher))

sendUDPFrame :: MVar Int -> Socket -> SockAddr -> Lazy.ByteString -> IO ()
sendUDPFrame writeLock mSocket peerSockAddr msg = do
    socketLock <- takeMVar writeLock
    Network.sendTo mSocket (Lazy.toStrict msg) peerSockAddr
    putMVar writeLock socketLock

createUDPFrame :: Lazy.ByteString -> Lazy.ByteString
createUDPFrame parcelSerialised = Lazy.concat [parcelSerialised]

doEncryptedHandshakeForUDP ::
       (MonadIO m, MonadBaseControl IO m, HasSecretKey m, HasAriviNetworkInstance m, Forall (Pure m))
    => Conn.IncompleteConnection
    -> PersonalityType
    -> m Conn.CompleteConnection
doEncryptedHandshakeForUDP connection pType = do
    sk <- getSecretKey
    ariviNetworkInstance <- getAriviNetworkInstance
    if Conn.personalityType connection == INITIATOR
        then do
            (ephemeralKeyPair, serialisedParcel) <-
                liftIO $ initiatorHandshake sk connection
            liftIO $
                atomically $
                writeTVar
                    (Conn.handshakeComplete connection)
                    Conn.HandshakeInitiated
        -- liftIO $ sendUDPFrame (Conn.waitWrite connection) (Conn.socket connection)
        --                                     (Conn.remoteSockAddr connection)
        --                                     (createUDPFrame serialisedParcel)
            liftIO $
                sendFrame
                    (Conn.waitWrite connection)
                    (Conn.socket connection)
                    (createUDPFrame serialisedParcel)
            -- (receivedMessage, peerSockAddr) <- liftIO $ Network.recvFrom (Conn.socket connection) 4096
            -- liftIO $ Network.sendAll (Conn.socket connection) (Lazy.toStrict $ createUDPFrame serialisedParcel)
            hsRespParcel <-
                liftIO $
                readHandshakeRespFromTChan
                    (Conn.inboundDatagramTChan connection)
            traceShow "inside INITIATOR" (return ())
            return
                (receiveHandshakeResponse
                     connection
                     ephemeralKeyPair
                     hsRespParcel)
        else do
            let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance
            parcel <-
                liftIO $
                atomically $ readTChan (Conn.inboundDatagramTChan connection)
            (serialisedParcel, updatedConn) <-
                liftIO $ recipientHandshake sk connection parcel
            traceShow "before sendUDPFrame HandshakeDone" (return ())
            -- _ <-
            --     liftIO $
            --     sendUDPFrame
            --         (Conn.waitWrite connection)
            --         (Conn.socket updatedConn)
            --         (Conn.remoteSockAddr updatedConn)
            --         (createUDPFrame serialisedParcel)
            liftIO $
                Network.sendTo
                    (Conn.socket updatedConn)
                    (Lazy.toStrict $ createUDPFrame serialisedParcel)
                    (Conn.remoteSockAddr updatedConn)
            traceShow "after sendUDPFrame HandshakeDone" (return ())
            let cid = Conn.connectionId updatedConn
            liftIO $
                atomically $
                writeTVar
                    (Conn.handshakeComplete updatedConn)
                    Conn.HandshakeDone
            liftIO $
                atomically $
                modifyTVar hashMapTVar (StrictHashMap.insert cid updatedConn)
            traceShow "end of doEncryptedHandshakeForUDP" (return ())
            return updatedConn

readFromReassemblyTChan :: TChan Parcel -> IO Parcel
readFromReassemblyTChan inboundDatagramTChan = do
    hsRespParcel <- atomically $ readTChan inboundDatagramTChan
    case hsRespParcel of
        parcel@(Parcel (HandshakeRespHeader _ _) _) -> return parcel
        _ -> throw AriviWrongParcelException

getPersonalityType :: Parcel -> PersonalityType
getPersonalityType receivedParcel =
    case receivedParcel of
        (Parcel (HandshakeInitHeader _ _) _) -> RECIPIENT
        (Parcel (HandshakeRespHeader _ _) _) -> INITIATOR
        _                                    -> error "Wrong PersonalityType"

readHandshakeRespFromTChan :: TChan Parcel -> IO Parcel
readHandshakeRespFromTChan inboundDatagramTChan = do
    tParcel <- readFromReassemblyTChan inboundDatagramTChan
    case tParcel of
        parcel@(Parcel (HandshakeRespHeader _ _) _) -> return parcel
        _ -> throw AriviWrongParcelException
