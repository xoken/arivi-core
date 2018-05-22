{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Arivi.Network.FSM
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides functions for Finite State Machine in Arivi Network
-- Layer
--
module Arivi.Network.FSM (
    -- * Datatypes
    -- Event(..)
    State(..)

  -- * FSM Functions
  , handleEvent
  , handleNextEvent
  , initFSM

  -- * Helper Functions
  -- , getEventType
  , getReplayNonce
  -- , takeLeft
  -- , takeRight

    -- * Post Timeouts Events
  , postDataParcelTimeOutEvent
  , postHandshakeInitTimeOutEvent
  , postPingParcelTimeOutEvent

    -- * Timers
  , dataMessageTimer
  , handshakeTimer
  , pingTimer

) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Network.Connection
import           Arivi.Network.Fragmenter
import           Arivi.Network.Handshake
import           Arivi.Network.OutboundDispatcher
import           Arivi.Network.StreamClient
import           Arivi.Network.Types
import           Arivi.Network.Utils
import           Arivi.P2P.Types                   (ServiceRequest (..),
                                                    ServiceType (..))
import           Control.Concurrent.Async
import           Control.Concurrent.Killable       (kill)
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Crypto.PubKey.Ed25519             (SecretKey)
import qualified Data.Binary                       as Binary (decode, encode)
import           Data.ByteString.Char8             as B (ByteString)
import           Data.Either
import qualified System.Timer.Updatable            as Timer (Delay, parallel)

-- | The different states that any thread in layer 1 can be in
data State =  Idle
            | KeyExchangeInitiated
            | SecureTransportEstablished
            | Terminated
            | Pinged
            deriving (Show, Eq)


-- | Handshake Timer is  30 seconds
handshakeTimer :: Timer.Delay
handshakeTimer = 3*(10^7)

-- | Data Message Timer 60 seconds
dataMessageTimer :: Timer.Delay
dataMessageTimer =  6*(10^7)

-- | Ping Message Timer is 30 seconds
pingTimer :: Timer.Delay
pingTimer =  3*(10^7)


-- | Initiate FSM
initFSM :: Connection -> IO State
initFSM connection =
    do
        nextEvent <- atomically $ readTChan (eventTChan connection)
        handleEvent connection Idle nextEvent

-- | Initial Aead nonce passed to the outboundFrameDispatcher
getAeadNonceInitiator :: B.ByteString
getAeadNonceInitiator = lazyToStrict $ Binary.encode (2::Integer)

-- | Initial Aead nonce passed to the outboundFrameDispatcher
getAeadNonceRecipient :: B.ByteString
getAeadNonceRecipient = lazyToStrict $ Binary.encode (2::Integer)

-- | Initial Aead nonce passed to the outboundFrameDispatcher
getReplayNonce :: Integer
getReplayNonce = 2


-- | This handles the state transition from one state of FSM to another
--   on the  basis of received events
handleEvent :: Connection -> State -> Event -> IO State

-- initiator will initiate the handshake
handleEvent connection Idle
                    (InitHandshakeEvent payload secretKey)  =
        do

            (serialisedParcel, updatedConn) <- initiatorHandshake secretKey
                                                                  connection
            -- These 2 lines should be done before calling initFSM?
            -- socket <- createSocket (ipAddress connection) (port connection)
                                                -- (transportType connection)
            -- Call async(listenIncomingMsgs socket (parcelTChan connection))

            -- Send the message
            sendFrame (socket updatedConn) (createFrame serialisedParcel)

            handshakeInitTimer <- Timer.parallel
                                    (postHandshakeInitTimeOutEvent updatedConn)
                                     handshakeTimer -- 30 seconds

            nextEvent <- atomically $ readTChan (eventTChan updatedConn)
            -- let nextEvent = getNextEvent updatedConn
            kill handshakeInitTimer
            handleEvent updatedConn KeyExchangeInitiated nextEvent

--recipient will go from Idle to VersionNegotiatedState
handleEvent connection Idle (KeyExchangeInitEvent parcel secretKey) =
        do
            -- Need to figure out what exactly to do with the fields like
            -- versionList, nonce and connectionId
            (serialisedParcel, updatedConn) <- recipientHandshake secretKey
                                                                  connection
                                                                  parcel
            -- Spawn an outboundFrameDsipatcher for sending out msgs
            -- How to get the aead and replay nonce that was used in the
            -- handshake?!
            async(outboundFrameDispatcher (outboundFragmentTChan updatedConn)
                                           updatedConn
                                           getAeadNonceInitiator
                                           getReplayNonce)
            -- Send the message back to the initiator
            sendFrame (socket updatedConn) (createFrame serialisedParcel)
            nextEvent <- atomically $ readTChan (eventTChan updatedConn)
            -- let nextEvent = getNextEvent updatedConn
            handleEvent updatedConn SecureTransportEstablished nextEvent


--initiator will go to SecureTransport from KeyExchangeInitiated state
handleEvent connection KeyExchangeInitiated
                    (KeyExchangeRespEvent parcel)  =
        do
            -- Need to figure out what exactly to do with the fields like
            -- versionList, nonce and connectionId
            let updatedConn = receiveHandshakeResponse connection parcel
            -- Spawn an outboundFrameDsipatcher for sending out msgs
            -- How to get the aead and replay nonce that was used in the
            -- handshake?!
            async(outboundFrameDispatcher (outboundFragmentTChan updatedConn)
                                           updatedConn
                                           getAeadNonceRecipient
                                           getReplayNonce)
            nextEvent <- atomically $ readTChan (eventTChan updatedConn)
            -- let nextEvent = getNextEvent updatedConn
            handleEvent updatedConn SecureTransportEstablished nextEvent

-- Receive message from the network
handleEvent connection SecureTransportEstablished (ReceiveDataEvent parcel) =
        -- do
            -- Insert into reassembly TChan
            -- atomically $ writeTChan (reassemblyTChan connection) parcel
            -- TODO handleDataMessage parcel --decodeCBOR - collectFragments -
            -- addToOutputChan
            handleNextEvent connection



-- Receive message from p2p layer
handleEvent connection SecureTransportEstablished
                            (SendDataEvent payload) =
        do
            -- Spawn a new thread for processing the payload
            async (processPayload payload connection)
            -- TODO chunk message, encodeCBOR, encrypt, send
            handleNextEvent connection


handleEvent connection SecureTransportEstablished
                                (TerminateConnectionEvent parcel) =

        handleEvent connection Terminated CleanUpEvent

handleEvent connection Terminated CleanUpEvent =
            --- do all cleanup here
            return Terminated

handleEvent connection KeyExchangeInitiated HandshakeTimeOutEvent =
            return Terminated

handleEvent connection Pinged DataTimeOutEvent =
        do
            pingTimer <- Timer.parallel (postPingParcelTimeOutEvent connection)
                                         pingTimer -- 30 seconds
            nextEvent <- atomically $ readTChan (eventTChan connection)
            -- let nextEvent = getNextEvent connection
            kill pingTimer
            handleEvent connection Pinged nextEvent


handleEvent connection Pinged PingTimeOutEvent=
         -- go to terminated
            return Terminated

handleEvent connection Pinged (ReceiveDataEvent parcel) =
         -- go to established state
        handleEvent connection SecureTransportEstablished
                                    (ReceiveDataEvent parcel)

handleEvent connection _ _  =
            handleEvent connection Terminated CleanUpEvent



-- getNextEvent :: Connection -> IO Event
-- getNextEvent connection = do
--             let serReqTChannel  = serviceReqTChan connection
--             let parcelTChannel = parcelTChan connection
--             let eitherEvent = readEitherTChan serReqTChannel parcelTChannel
--             e <- liftIO $ atomically eitherEvent

--             if isLeft e
--                 then
--                     do
--                         let service = serviceType (takeLeft e)

--                         case service of
--                             -- OPEN  -> return (InitHandshakeEvent
--                                                             -- (takeLeft e))
--                             CLOSE -> return (TerminateConnectionEvent
--                                                             (takeLeft e))
--                             SENDMSG -> return (SendDataEvent
--                                                             (takeLeft e))
--                             HANDSHAKE_TIMEOUT -> return
--                                                 (HandshakeInitTimeOutEvent
--                                                             (takeLeft e))
--                             DATA_TIMEOUT -> return (DataParcelTimeOutEvent
--                                                             (takeLeft e))
--                             PING_TIMEOUT -> return (PingTimeOutEvent
--                                                             (takeLeft e))
--             else
--                 do
--                     let opcodeType = opcode (header (takeRight e))

--                     case opcodeType of
--                         -- KEY_EXCHANGE_INIT -> return (KeyExchangeInitEvent
--                                                     -- (takeRight e))
--                         KEY_EXCHANGE_RESP -> return (KeyExchangeRespEvent
--                                                     (takeRight e))
--                         DATA -> return (ReceiveDataEvent (takeRight e))
--                         PING -> return (PINGDataEvent (takeRight e))
--                         PONG -> return (PONGDataEvent (takeRight e))




-- readEitherTChan  ::  TChan a -> TChan b -> STM (Either a b)
-- readEitherTChan a b =
--                 fmap  Left (readTChan a)
--                 `orElse`
--                 fmap  Right (readTChan b)


-- takeLeft :: Either a b -> a
-- takeLeft (Left left) = left

-- takeRight :: Either a b -> b
-- takeRight (Right right) = right

postHandshakeInitTimeOutEvent :: Connection -> IO ()
postHandshakeInitTimeOutEvent connection = do
    let eventTChannel = eventTChan connection
    atomically $ writeTChan eventTChannel HandshakeTimeOutEvent


postDataParcelTimeOutEvent :: Connection -> IO ()
postDataParcelTimeOutEvent connection = do
    let eventTChannel = eventTChan connection
    atomically $ writeTChan eventTChannel DataTimeOutEvent

postPingParcelTimeOutEvent :: Connection -> IO ()
postPingParcelTimeOutEvent connection = do
    let eventTChannel = eventTChan connection
    atomically $ writeTChan eventTChannel PingTimeOutEvent


-- getEventType :: Event -> ServiceRequest -> Event
-- getEventType inputEvent = case inputEvent of
--                          (DataParcelTimeOutEvent serviceRequest)
--                             -> DataParcelTimeOutEvent

handleNextEvent :: Connection -> IO State
handleNextEvent connection = do
    dataMessageTimer <- Timer.parallel (postDataParcelTimeOutEvent connection)
                                        dataMessageTimer -- 60 seconds

    nextEvent <- atomically $ readTChan (eventTChan connection)
    -- let nextEvent = getNextEvent updatedConn

    kill dataMessageTimer -- is killing died event gives any exception
    case nextEvent of
        DataTimeOutEvent
            -> handleEvent connection Pinged nextEvent
        _  -> handleEvent connection SecureTransportEstablished nextEvent

-- | TODO replace this with actual decryption function
-- decryptParcel parcel = KeyExParcel undefined undefined
--                                           undefined undefined

-- test :: IO (Async State)
-- test = do
--         <- atomically newTChan
--         -- connection <- atomically newTChan

--         -- let serviceRequest = ServiceRequest OPEN "Message" -- "IP" "Port"
                                                                 -- "NodeId"
--         serviceContexTChan <- writeTChan serviceContexTChan serviceRequest

--         async (handleEvent serviceContexTChan connection Idle
--                                 (InitServiceNegotiationEvent serviceRequest))




-- TODO implement all the following functions

-- TODO encodeCBOR
encodeCBOR = print "Encoding CBOR"
-- TODO decodeCBOR
decodeCBOR = print "Decoding CBOR"
-- TODO defragment
defragment = print "Defragmenting"
-- TODO fragment
fragment = print "Fragmenting"
-- TODO multiplex
multiplex = print "Multiplexing"
-- TODO demultiplex
demultiplex1 = print "Demultiplexing"
