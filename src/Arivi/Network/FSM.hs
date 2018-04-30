module Arivi.Network.FSM (

) where

import           Arivi.Network.Connection
import           Arivi.Network.Types
import           Arivi.P2P.Types          (ServiceRequest (..),
                                           ServiceType (..))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Either


-- The different states that any thread in layer 1 can be in
data State =  Idle
            | VersionInitiated
            | VersionNegotiated
            | KeyExchangeInitiated
            | SecureTransportEstablished
            | Terminated
            deriving (Show, Eq)

-- The different events in layer 1 that cause state change
data Event =  InitHandshakeEvent {serviceRequest::ServiceRequest}
             | TerminateConnectionEvent {serviceRequest::ServiceRequest}
             | SendDataEvent {serviceRequest::ServiceRequest}
             | VersionNegotiationInitEvent {parcel::Parcel}
             | VersionNegotiationRespEvent {parcel::Parcel}
             | KeyExchangeInitEvent {parcel::Parcel}
             | KeyExchangeRespEvent {parcel::Parcel}
             | ReceiveDataEvent {parcel::Parcel}
             | CleanUpEvent
          deriving (Show)


handle :: Connection -> State -> Event -> IO State

-- initiator will initiate the handshake
handle connection Idle
                    (InitHandshakeEvent serviceRequest)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handle connection VersionInitiated

--recipient will go from Idle to VersionNegotiatedState
handle connection Idle (VersionNegotiationInitEvent parcel) =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handle connection VersionNegotiated


--initiator will go from VersionInitiated to KeyExchangeInitiatedState
-- (since VersionNegotiated is a transient event for initiator)
handle connection VersionInitiated
                    (VersionNegotiationRespEvent parcel)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handle connection VersionNegotiated

-- initiator will to KeyExchangeInitiated state
handle connection VersionNegotiated (KeyExchangeInitEvent parcel) =
    case peerType connection of
        INITIATOR -> handle connection KeyExchangeInitiated (KeyExchangeInitEvent parcel)
        RECIPIENT -> do
                let nextEvent = getNextEvent connection
                nextEvent >>= handle connection SecureTransportEstablished


--initiator will go to SecureTransport from KeyExchangeInitiated state
handle connection KeyExchangeInitiated
                    (KeyExchangeRespEvent parcel)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handle connection SecureTransportEstablished

-- Receive message from the network
handle connection SecureTransportEstablished (ReceiveDataEvent parcel) =
        do
            let nextEvent = getNextEvent connection
            -- TODO handleDataMessage parcel --decodeCBOR - collectFragments -
            -- addToOutputChan
            nextEvent >>= handle connection SecureTransportEstablished

-- Receive message from p2p layer
handle connection SecureTransportEstablished (SendDataEvent serviceRequest) =
        do
            -- TODO chunk message, encodeCBOR, encrypt, send
            let nextEvent = getNextEvent connection
            nextEvent >>= handle connection SecureTransportEstablished

handle connection SecureTransportEstablished (TerminateConnectionEvent parcel) =
        handle connection Terminated CleanUpEvent

handle connection Terminated CleanUpEvent =
            --- do all cleanup here
            return Terminated

handle connection _ _  =
            handle connection Terminated CleanUpEvent



getNextEvent
  :: Control.Monad.IO.Class.MonadIO m => Connection -> m Event

getNextEvent connection = do
            let serReqTChannel  = serviceReqTChan connection
            let parcelTChannel = parcelTChan connection
            let eitherEvent = readEitherTChan serReqTChannel parcelTChannel
            e <- liftIO $ atomically eitherEvent

            if isLeft e
                then
                    do
                        let service = serviceType (takeLeft e)

                        case service of
                            OPEN  -> return (InitHandshakeEvent
                                                            (takeLeft e))
                            CLOSE -> return (TerminateConnectionEvent
                                                            (takeLeft e))
                            SENDMSG -> return (SendDataEvent
                                                            (takeLeft e))
            else
                do
                    let opcodeType = opcode (takeRight e)

                    case opcodeType of
                        VERSION_INIT -> return (VersionNegotiationInitEvent (takeRight e))
                        VERSION_RESP  -> return (VersionNegotiationRespEvent (takeRight e))
                        KEY_EXCHANGE_INIT -> return (KeyExchangeInitEvent (takeRight e))
                        KEY_EXCHANGE_RESP -> return (KeyExchangeRespEvent (takeRight e))
                        DATA -> return (ReceiveDataEvent (takeRight e))




readEitherTChan  ::  TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
                fmap  Left (readTChan a)
                `orElse`
                fmap  Right (readTChan b)


takeLeft :: Either a b -> a
takeLeft (Left left) = left

takeRight :: Either a b -> b
takeRight (Right right) = right



-- test :: IO (Async State)
-- test = do
--         <- atomically newTChan
--         -- connection <- atomically newTChan

--         -- let serviceRequest = ServiceRequest OPEN "Message" -- "IP" "Port" "NodeId"
--         serviceContexTChan <- writeTChan serviceContexTChan serviceRequest

--         async (handle serviceContexTChan connection Idle
--                                   (InitServiceNegotiationEvent serviceRequest))




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
