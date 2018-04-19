module Arivi.Network.Layer2.FSM (

) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Either


data State = Idle
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


          deriving (Show, Eq)


data ServiceType =  OPEN
                  | CLOSED
                  | SENDMSG
                  deriving (Show,Eq)


type Message = String
type IP = String
type Port = String
type NodeId = String

data ServiceRequest = ServiceRequest {
                         serviceType :: ServiceType
                        ,message     :: Message
                        ,ip          :: IP
                        ,port        :: Port
                        ,nodeid      :: NodeId
                    } deriving (Show,Eq)



data Opcode = ERROR
            | DATA
            | OFFER
            | ANSWER
            deriving (Show,Eq)


newtype Frame = Frame {
                opcode :: Opcode
            } deriving (Show,Eq)

handle :: TChan ServiceRequest -> TChan Frame -> State -> Event -> IO State

--initiator
handle serviceRequestTChan frameTchan Idle
                    (InitServiceNegotiationEvent serviceRequest)  =
        do
            print "Inside:handle Idle InitServiceNegotiationEvent"

            let nextEvent = getNextEvent serviceRequestTChan frameTchan

            nextEvent >>= handle serviceRequestTChan frameTchan Offered


handle serviceRequestTChan frameTchan Offered
                    (AnswerMessageEvent frame)  =
        do

            let nextEvent = getNextEvent serviceRequestTChan frameTchan

            nextEvent >>= handle serviceRequestTChan frameTchan Established




--recipient
handle serviceRequestTChan frameTchan Idle (OfferMessageEvent frame) =
        do

            let nextEvent = getNextEvent serviceRequestTChan frameTchan
            -- Send an answer
            nextEvent >>= handle serviceRequestTChan frameTchan Established


handle serviceRequestTChan frameTchan Established (DataMessageEvent frame) =
        do
            let nextEvent = getNextEvent serviceRequestTChan frameTchan
            -- TODO handleDataMessage frame --decodeCBOR - collectFragments -
            -- addToOutputChan
            nextEvent >>= handle serviceRequestTChan frameTchan Established


handle serviceRequestTChan frameTchan
            Established (TerminateConnectionEvent frame)=
        handle serviceRequestTChan frameTchan Terminated CleanUpEvent


handle serviceRequestTChan frameTchan Terminated CleanUpEvent =
            --- do all cleanup here
            return Terminated



handle serviceRequestTChan frameTchan _ _  =
            handle serviceRequestTChan frameTchan Terminated CleanUpEvent


getNextEvent
  :: Control.Monad.IO.Class.MonadIO m =>
     TChan ServiceRequest -> TChan Frame -> m Event

getNextEvent serviceRequestTChan frameTchan = do
            let eitherEvent = readEitherTChan serviceRequestTChan frameTchan

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
getOpcode (Frame opcode) = opcode

getServiceType:: ServiceRequest -> ServiceType
getServiceType (ServiceRequest serviceType _ _ _ _ ) = serviceType


test :: IO (Async State)
test = do
        serviceRequestTChan <- atomically newTChan
        frameTchan <- atomically newTChan

        let serviceRequest = ServiceRequest OPEN "Message" "IP" "Port" "NodeId"

        async (handle serviceRequestTChan frameTchan Idle
                                  (InitServiceNegotiationEvent serviceRequest))




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
