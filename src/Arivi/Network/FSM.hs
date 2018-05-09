module Arivi.Network.FSM (
    Event(..)
  , State(..)
  , handleEvent
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
             | VersionNegotiationInitEvent {parcelCipher::ParcelCipher}
             | VersionNegotiationRespEvent {parcelCipher::ParcelCipher}
             | KeyExchangeInitEvent {parcelCipher::ParcelCipher}
             | KeyExchangeRespEvent {parcelCipher::ParcelCipher}
             | ReceiveDataEvent {parcelCipher::ParcelCipher}
             | CleanUpEvent
          deriving (Show)


handleEvent :: Connection -> State -> Event -> IO State

-- initiator will initiate the handshake
handleEvent connection Idle
                    (InitHandshakeEvent serviceRequest)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handleEvent connection VersionInitiated

--recipient will go from Idle to VersionNegotiatedState
handleEvent connection Idle (VersionNegotiationInitEvent parcelCipher) =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handleEvent connection VersionNegotiated


--initiator will go from VersionInitiated to KeyExchangeInitiatedState
-- (since VersionNegotiated is a transient event for initiator)
handleEvent connection VersionInitiated
                    (VersionNegotiationRespEvent parcelCipher)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handleEvent connection VersionNegotiated

-- initiator will to KeyExchangeInitiated state
handleEvent connection VersionNegotiated (KeyExchangeInitEvent parcelCipher) =
    case peerType connection of
        INITIATOR -> handleEvent connection KeyExchangeInitiated (KeyExchangeInitEvent parcelCipher)
        RECIPIENT -> do
                let nextEvent = getNextEvent connection
                nextEvent >>= handleEvent connection SecureTransportEstablished


--initiator will go to SecureTransport from KeyExchangeInitiated state
handleEvent connection KeyExchangeInitiated
                    (KeyExchangeRespEvent parcelCipher)  =
        do
            let nextEvent = getNextEvent connection
            nextEvent >>= handleEvent connection SecureTransportEstablished

-- Receive message from the network
handleEvent connection SecureTransportEstablished (ReceiveDataEvent parcelCipher) =
        do
            let nextEvent = getNextEvent connection
            -- TODO handleDataMessage parcelCipher --decodeCBOR - collectFragments -
            -- addToOutputChan
            nextEvent >>= handleEvent connection SecureTransportEstablished

-- Receive message from p2p layer
handleEvent connection SecureTransportEstablished (SendDataEvent serviceRequest) =
        do
            -- TODO chunk message, encodeCBOR, encrypt, send
            let nextEvent = getNextEvent connection
            nextEvent >>= handleEvent connection SecureTransportEstablished

handleEvent connection SecureTransportEstablished (TerminateConnectionEvent parcelCipher) =
        handleEvent connection Terminated CleanUpEvent

handleEvent connection Terminated CleanUpEvent =
            --- do all cleanup here
            return Terminated

handleEvent connection _ _  =
            handleEvent connection Terminated CleanUpEvent



getNextEvent
  :: Control.Monad.IO.Class.MonadIO m => Connection -> m Event

getNextEvent connection = do
            let serReqTChannel  = serviceReqTChan connection
            let parcelCipherTChannel = parcelCipherTChan connection
            let eitherEvent = readEitherTChan serReqTChannel parcelCipherTChannel
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
                    let opcodeType = opcode (decryptParcelCipher(takeRight e))

                    case opcodeType of
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


-- | TODO replace this with actual decryption function
decryptParcelCipher parcelCipher = KeyExParcel undefined undefined
                                          undefined undefined

-- test :: IO (Async State)
-- test = do
--         <- atomically newTChan
--         -- connection <- atomically newTChan

--         -- let serviceRequest = ServiceRequest OPEN "Message" -- "IP" "Port" "NodeId"
--         serviceContexTChan <- writeTChan serviceContexTChan serviceRequest

--         async (handleEvent serviceContexTChan connection Idle
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
