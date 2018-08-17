{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      :  Arivi.Network.StreamDatagram
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing connections in Arivi
-- communication for Datagrams
module Arivi.Network.DatagramServer
    ( runUdpServer
    ) where

import           Arivi.Env
import           Arivi.Network.Connection        as Conn
import           Arivi.Network.ConnectionHandler (closeConnection,
                                                  establishSecureConnection,
                                                  readUdpSock, sendUdpMessage)
import           Arivi.Network.Types             (ConnectionHandle (..), NodeId,
                                                  TransportType,
                                                  deserialiseOrFail)
                                                --   Parcel,

import           Arivi.Network.Exception
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Concurrent.Async.Lifted (async)
import           Control.Exception.Lifted        (finally, throw)
import           Control.Monad                   (forever)
import           Control.Monad.IO.Class
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Lazy            (fromStrict)
import qualified Data.Text                       as T
-- import           Data.Time.Clock
import           Data.Time.Units
import           Network.Socket                  hiding (close, recv, recvFrom,
                                                  send)
import qualified Network.Socket
import           Network.Socket.ByteString       hiding (recv, send)
import           System.CPUTime
makeSocket :: ServiceName -> SocketType -> IO Socket
makeSocket portNumber socketType = do
    let hint =
            defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = socketType}
    selfAddr:_ <- getAddrInfo (Just hint) Nothing (Just portNumber)
    sock <-
        Network.Socket.socket
            (addrFamily selfAddr)
            (addrSocketType selfAddr)
            (addrProtocol selfAddr)
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    bind sock (addrAddress selfAddr)
    return sock

runUdpServer ::
       (HasSecretKey m, HasLogging m,HasStatsdClient m)
    => ServiceName
    -> (NodeId -> TransportType -> ConnectionHandle -> m ())
    -> m ()
runUdpServer portNumber handler =
    $(withLoggingTH) (LogNetworkStatement "UDP Server started...") LevelDebug $ do
        mSocket <- liftIO $ makeSocket portNumber Datagram
        finally
            (forever $ do
                 (msg, peerSockAddr) <- liftIO $ recvFrom mSocket 4096
                 mSocket' <- liftIO $ makeSocket portNumber Datagram
                 liftIO $ connect mSocket' peerSockAddr
                 async (newUdpConnection msg mSocket' handler))
            (liftIO
                 (print ("So long and thanks for all the fish." :: String) >>
                  Network.Socket.close mSocket))

newUdpConnection ::
       forall m. (HasSecretKey m, HasLogging m,HasStatsdClient m)
    => ByteString
    -> Socket
    -> (NodeId -> TransportType -> ConnectionHandle -> m ())
    -> m ()
newUdpConnection hsInitMsg sock handler =
    liftIO (getPeerName sock) >>= \addr ->
        $(withLoggingTH)
        (LogNetworkStatement $
            T.append (T.pack "newUdpConnection latest: ") (T.pack (show addr)))
            LevelDebug $
        either
            (throw . NetworkDeserialiseException)
            (\hsInitParcel -> do
                incrementCounter "Incoming connection attempted"
                initTime <- liftIO getCPUTime
                sk <- getSecretKey
                conn <-
                    liftIO $ establishSecureConnection sk sock id hsInitParcel
                handler
                    (Conn.remoteNodeId conn)
                    (Conn.transportType conn)
                    ConnectionHandle
                        { send = sendUdpMessage conn
                        , recv = readUdpSock conn
                        , close = closeConnection (Conn.socket conn)
                        }
                finalTime <- liftIO getCPUTime
                let diff = (fromIntegral ( (finalTime - initTime) `div` (1000000000)))
                time "Connection establishment time" $ (diff ::Millisecond)
                incrementCounter "Incoming Connection Established")

            (deserialiseOrFail (fromStrict hsInitMsg))
