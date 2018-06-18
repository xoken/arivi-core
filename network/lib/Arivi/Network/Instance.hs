{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Arivi.Network.Instance
    ( AriviNetworkInstance(..)
    , NetworkConfig(..)
    , NetworkHandle(..)
    , closeConnection
    , connectionMap
    , lookupCId
    , mkAriviNetworkInstance
    , openConnection
    , sendMessage
    ) where

import           Arivi.Crypto.Utils.PublicKey.Utils   (encryptMsg)
import           Arivi.Crypto.Utils.Random
import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection             as Conn
import           Arivi.Network.ConnectionHandler
import           Arivi.Network.Datagram
import           Arivi.Network.Fragmenter
import           Arivi.Network.Handshake
import           Arivi.Network.StreamClient
import           Arivi.Network.Types                  as ANT (AeadNonce,
                                                              ConnectionId,
                                                              Event (..),
                                                              Header (..),
                                                              NodeId,
                                                              OutboundFragment,
                                                              Parcel (..),
                                                              Payload (..),
                                                              PersonalityType,
                                                              SequenceNum,
                                                              TransportType (..))
import           Arivi.Network.Utils
import           Arivi.Utils.Exception
import           Codec.Serialise
import           Control.Concurrent                   (newMVar, threadDelay)
import           Control.Concurrent.Async.Lifted.Safe
import           Control.Concurrent.Killable          (kill)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan         (TChan)
import           Control.Exception                    (SomeException, throw,
                                                       try)
import qualified Control.Exception.Lifted             as CEL (try)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.STM                    (atomically)
import           Crypto.PubKey.Ed25519                (SecretKey)
import qualified Data.ByteString                      as B
import           Data.ByteString.Lazy                 as L
import           Data.HashMap.Strict                  as HM
import           Data.Int                             (Int16, Int64)
import           Data.Maybe                           (fromMaybe)
import           Debug.Trace
import           Network.Socket

-- | Strcuture to hold the arivi configurations can also contain more
--   parameters but for now just contain 3
data NetworkConfig = NetworkConfig
    { hostip  :: String
    , udpport :: String
    , tcpPort :: String
                    -- , TODO   transportType :: TransportType and only one port
    } deriving (Show)

-- | Strcuture which holds all the information about a running arivi Instance
--   and can be passed around to different functions to differentiate betweeen
--   different instances of arivi.
newtype NetworkHandle = NetworkHandle
    { ariviUDPSock :: (Socket, SockAddr)
    }-- ,   ariviTCPSock :: (Socket,SockAddr)
                    -- ,   udpThread    :: MVar ThreadId
                    -- ,   tcpThread    :: MVar ThreadId
                    -- ,
                    -- registry     :: MVar MP.ServiceRegistry

doEncryptedHandshake ::
       Conn.IncompleteConnection -> SecretKey -> IO Conn.CompleteConnection
doEncryptedHandshake connection sk = do
    (ephemeralKeyPair, serialisedParcel) <- initiatorHandshake sk connection
    sendFrame
        (Conn.waitWrite connection)
        (Conn.socket connection)
        (createFrame serialisedParcel)
    hsRespParcel <-
        readHandshakeRespSock
            (Conn.waitWrite connection)
            (Conn.socket connection)
            sk
    return $ receiveHandshakeResponse connection ephemeralKeyPair hsRespParcel

openConnection ::
       ( HasAriviNetworkInstance m
       , HasSecretKey m
       , HasLogging m
       , Forall (Pure m)
       )
    => HostName
    -> PortNumber
    -> TransportType
    -> NodeId
    -> PersonalityType
    -> m (Either AriviException ANT.ConnectionId)
openConnection addr port tt rnid pType = do
    ariviInstance <- getAriviNetworkInstance
    let cId = makeConnectionId addr port tt
    let tv = connectionMap ariviInstance
    hm <- liftIO $ readTVarIO tv
    case HM.lookup cId hm of
        Just conn -> return $ Right cId
        Nothing -> do
            sk <- getSecretKey
            socket <- liftIO $ createSocket addr (read (show port)) tt
            p2pMsgTChan <- liftIO (newTChanIO :: IO (TChan ByteString))
            egressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
            ingressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
            aeadNonce <- liftIO (newTVarIO (2 :: AeadNonce))
            writeLock <- liftIO $ newMVar 0
            hsCompleteTVar <- liftIO $ newTVarIO Conn.HandshakeNotStarted
            mInboundDatagramTChan <- liftIO $ atomically newTChan
            let connection =
                    mkIncompleteConnection'
                    { Conn.connectionId = cId
                    , Conn.remoteNodeId = rnid
                    , Conn.ipAddress = addr
                    , Conn.port = port
                    , Conn.transportType = tt
                    , Conn.personalityType = pType
                    , Conn.socket = socket
                    , Conn.inboundDatagramTChan = mInboundDatagramTChan
                    , Conn.waitWrite = writeLock
                    , Conn.p2pMessageTChan = p2pMsgTChan
                    , Conn.egressSeqNum = egressNonce
                    , Conn.ingressSeqNum = ingressNonce
                    , Conn.aeadNonceCounter = aeadNonce
                    , Conn.handshakeComplete = hsCompleteTVar
                    }
            if tt == TCP
                then do
                    res <- liftIO $ try $ doEncryptedHandshake connection sk
                    case res of
                        Left e -> return $ Left e
                        Right updatedConn -> do
                            liftIO $
                                atomically $
                                modifyTVar tv (HM.insert cId updatedConn)
                      -- async (readSock updatedConn HM.empty)
                            async (readSock' updatedConn)
                            return $ Right cId
                else do
                    traceShow "before Handshake" (return ())
                    res <- CEL.try $ doEncryptedHandshakeForUDP connection pType
                    traceShow "after Handshake" (return ())
                    case res of
                        Left e -> return $ Left e
                        Right updatedConn -> do
                            liftIO $
                                atomically $
                                modifyTVar tv (HM.insert cId updatedConn)
                      -- async (readSock updatedConn HM.empty)
                      -- async (readSock' updatedConn)
                            return $ Right cId

sendMessage ::
       (HasAriviNetworkInstance m, HasLogging m)
    => ANT.ConnectionId
    -> ByteString
    -> m ()
sendMessage cId msg =
    $(withLoggingTH) (LogNetworkStatement "Sending Message: ") LevelInfo $ do
        connectionOrFail <- lookupCId cId
        case connectionOrFail of
            Nothing -> throw AriviInvalidConnectionIdException
            Just conn -> do
                let sock = Conn.socket conn
                    lock = Conn.waitWrite conn
                fragments <- liftIO $ processPayload (Payload msg) conn
                mapM_
                    (\frame ->
                         liftIO
                             (atomically frame >>= (try . sendFrame lock sock)) >>= \case
                             Left (_ :: SomeException) ->
                                 closeConnection cId >>
                                 throw AriviSocketException
                             Right _ -> return ())
                    fragments

closeConnection :: (HasAriviNetworkInstance m) => ANT.ConnectionId -> m ()
closeConnection cId = do
    ariviInstance <- getAriviNetworkInstance
    let tv = connectionMap ariviInstance
    hmap <- liftIO $ readTVarIO tv
    let connOrFail = HM.lookup cId hmap
    case connOrFail of
        Nothing -> return ()
        Just conn ->
            liftIO (close (Conn.socket conn)) >>
            liftIO (atomically $ modifyTVar tv (HM.delete cId))

lookupCId ::
       (HasAriviNetworkInstance m)
    => ANT.ConnectionId
    -> m (Maybe CompleteConnection)
lookupCId cId = do
    ariviInstance' <- getAriviNetworkInstance
    let tv = connectionMap ariviInstance'
    hmap <- liftIO $ readTVarIO tv
    return $ HM.lookup cId hmap
