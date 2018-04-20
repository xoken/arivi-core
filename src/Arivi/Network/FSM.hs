module Arivi.Network.FSM (

) where

import           Arivi.Network.Layer2.Connection
import           Arivi.Network.Layer2.ServiceRegistry
import           Arivi.Network.Types                  -- (Frame (HandshakeFrame),Opcode)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Either

data State =  Idle
            | Offered
            | Established
            | Terminated
            deriving (Show, Eq)

data Event =  InitServiceNegotiationEvent
                 {serviceRequest::ServiceRequest}
             | TerminateConnectionEvent {serviceRequest::ServiceRequest}
             | SendDataEvent {serviceRequest::ServiceRequest}
             | CleanUpEvent

             | OfferMessageEvent {frame::Frame}
             | AnswerMessageEvent {frame::Frame}
             | ErrorMessageEvent {frame::Frame}
             | DataMessageEvent {frame::Frame}
             deriving (Show)


-- data ServiceType =  OPEN
--                   | CLOSED
--                   | SENDMSG
--                   deriving (Show,Eq)


type Message = String

-- data ServiceRequest = ServiceRequest {
--                         serviceType :: ServiceType
--                        -- ,connection  :: Connection
--                        ,message     :: Message
--                       } deriving (Show,Eq)



-- data Opcode = ERROR
--             | DATA
--             | OFFER
--             | ANSWER
--             deriving (Show,Eq)


-- newtype Frame = Frame {
--                 opcode :: Opcode
--             } deriving (Show,Eq)

handle :: ServiceContext -> Connection -> State -> Event -> IO State

--initiator
handle serviceContext connection Idle
                    (InitServiceNegotiationEvent serviceRequest)  =
        do
            print "Inside:handle Idle InitServiceNegotiationEvent"

            let nextEvent = getNextEvent serviceContext connection

            nextEvent >>= handle serviceContext connection Offered


handle serviceContext connection Offered
                    (AnswerMessageEvent frame)  =
        do

            let nextEvent = getNextEvent serviceContext connection

            nextEvent >>= handle serviceContext connection Established




--recipient
handle serviceContext connection Idle (OfferMessageEvent frame) =
        do

            let nextEvent = getNextEvent serviceContext connection
            -- Send an answer
            nextEvent >>= handle serviceContext connection Established


handle serviceContext connection Established (DataMessageEvent frame) =
        do
            let nextEvent = getNextEvent serviceContext connection
            -- TODO handleDataMessage frame --decodeCBOR - collectFragments -
            -- addToOutputChan
            nextEvent >>= handle serviceContext connection Established


handle serviceContext connection
            Established (TerminateConnectionEvent frame)=
        handle serviceContext connection Terminated CleanUpEvent


handle serviceContext connection Terminated CleanUpEvent =
            --- do all cleanup here
            return Terminated



handle serviceContext connection _ _  =
            handle serviceContext connection Terminated CleanUpEvent


getNextEvent
  :: Control.Monad.IO.Class.MonadIO m =>
     ServiceContext -> Connection -> m Event

getNextEvent serviceContext connection = do
            let serviceRequestTChan = getServiceRequestTChan connection
            let frameTChan = getFrameTChan connection
            let eitherEvent = readEitherTChan serviceRequestTChan frameTChan

            e <- liftIO $ atomically eitherEvent

            if isLeft e
                then
                    do
                        let serviceType = getServiceType (takeLeft e)

                        case serviceType of
                            OPEN -> return (InitServiceNegotiationEvent
                                                            (takeLeft e))
                            CLOSED -> return (TerminateConnectionEvent
                                                            (takeLeft e))
                            SENDMSG -> return (SendDataEvent
                                                            (takeLeft e))
            else
                do
                    let opcode = getOpcode (takeRight e)

                    case opcode of
                        ERROR  -> return (ErrorMessageEvent (takeRight e))
                        DATA   -> return (DataMessageEvent (takeRight e))
                        OFFER  -> return (OfferMessageEvent (takeRight e))
                        ANSWER -> return (AnswerMessageEvent (takeRight e))




readEitherTChan  ::  TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
                fmap  Left (readTChan a)
                `orElse`
                fmap  Right (readTChan b)


takeLeft :: Either a b -> a
takeLeft (Left left) = left

takeRight :: Either a b -> b
takeRight (Right right) = right


getOpcode :: Frame -> Opcode
getOpcode (HandshakeFrame _ opcode _ _ _ _ _ _) = opcode

getServiceType:: ServiceRequest -> ServiceType
getServiceType (ServiceRequest serviceType _  ) = serviceType


getServiceRequestTChan (Connection _ _ _ _ _ serviceRequestTChan _) =
                                                           serviceRequestTChan


getFrameTChan (Connection _ _ _ _ _ _ frame) = frame


-- test :: IO (Async State)
-- test = do
--         serviceContext <- atomically newTChan
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
demultiplex = print "Demultiplexing"
