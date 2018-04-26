-- |
-- Module      :  Arivi.P2P.FSM
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides Finite State Machine for running Arivi P2P protocol

module Arivi.P2P.FSM (
     Event(..)
   , State(..)
   , getNextEvent
   , getOpcode
   , getP2PRequestTChan
   , getServiceType
   , handle
   , readEitherTChan
   , takeLeft
   , takeRight

) where

import           Arivi.P2P.Connection      (Connection (..))
import           Arivi.P2P.ServiceRegistry (ServiceContext (..))
import           Arivi.P2P.Types           (Message (..), Opcode (..),
                                            P2PRequest (..), ServiceType (..))
import           Control.Concurrent.Async  (async)
import           Control.Concurrent.STM    (STM, TChan, atomically, orElse,
                                            readTChan)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Either               (Either (Left, Right), isLeft,
                                            isRight)



-- | These are the states of Finite State Machine.
data State =  Idle        -- ^ Idle state of Finite State Machine
            | Offered     -- ^ Offered state indicates the Negotiation if
                          --   offered
            | Established -- ^ On Successful completion of
                          --  `InitServiceNegotiationEvent` FSM goes to
                          --   Established state. At this state FSM is ready to
                          --   Exchange Data
            | Terminated  -- ^ On receiving `CLOSED` serviceType or On receiving
                          --  `ERROR` frame FSM will go to Terminated State
            deriving (Show, Eq)



-- | These are the events of Finite State Machine for state transition.
data Event =  InitServiceNegotiationEvent {serviceRequest::P2PRequest}
              -- ^ Event that Initiates the Service Negotiation
             | TerminateConnectionEvent {serviceRequest::P2PRequest}
              -- ^ Event that Terminates the Connection
             | SendDataEvent {serviceRequest::P2PRequest}
              -- ^ Event used for sending data
             | CleanUpEvent
              -- ^ Event that CleanUps the Connection
             | OfferMessageEvent {frame::Message}
              -- ^ Show the give message is of an Offer
             | AnswerMessageEvent {frame::Message}
              -- ^ Show the give message is of an Answer for given Offer
             | ErrorMessageEvent {frame::Message}
              -- ^ Show the give message is of an Error Type
             | DataMessageEvent {frame::Message}
              -- ^ This Message is a Normal Data Exchange Type
             deriving (Show)





-- |  This handles the state transition from one state of FSM to another
--    on the  basis of received events
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


-- | This gives nextEvent for FSM transition
getNextEvent
  :: Control.Monad.IO.Class.MonadIO m =>
     ServiceContext -> Connection -> m Event

getNextEvent serviceContext connection = do
            let serviceRequestTChan = getP2PRequestTChan connection
            let frameTChan = getMessageTChan connection
            let eitherEvent = readEitherTChan serviceRequestTChan frameTChan

            e <- liftIO $ atomically eitherEvent

            if isLeft e
                then
                    do
                        let service = getServiceType (takeLeft e)

                        case service of
                            OPEN -> return (InitServiceNegotiationEvent
                                                            (takeLeft e))
                            CLOSED -> return (TerminateConnectionEvent
                                                            (takeLeft e))
                            SENDMSG -> return (SendDataEvent
                                                            (takeLeft e))
            else
                do
                    let opcodeType = getOpcode (takeRight e)

                    case opcodeType of
                        ERROR  -> return (ErrorMessageEvent (takeRight e))
                        DATA   -> return (DataMessageEvent (takeRight e))
                        OFFER  -> return (OfferMessageEvent (takeRight e))
                        ANSWER -> return (AnswerMessageEvent (takeRight e))



-- | Reads from TChans whichever is present
readEitherTChan  ::  TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
                fmap  Left (readTChan a)
                `orElse`
                fmap  Right (readTChan b)

-- | Give the left part of Either a b
takeLeft :: Either a b -> a
takeLeft (Left left) = left

-- | Give the right part of Either a b
takeRight :: Either a b -> b
takeRight (Right right) = right

-- | Takes Message as input and extracts Opcode from it.
getOpcode :: Message -> Opcode
getOpcode = opcode

-- | Takes P2PRequest as input and extracts ServiceType from it.
getServiceType:: P2PRequest -> ServiceType
getServiceType = serviceType



-- | Takes Connection as input and extracts TChan P2PRequest from it.
getP2PRequestTChan :: Connection -> TChan P2PRequest
getP2PRequestTChan = serviceRequestTChan

-- | Takes Connection as input and extracts TChan Message from it.
getMessageTChan :: Connection -> TChan Message
getMessageTChan = messageTChan

-- test :: IO (Async State)
-- test = do
--         serviceContext <- atomically newTChan
         -- connection <- atomically newTChan

--     -- let serviceRequest = P2PRequest OPEN "Message" -- "IP" "Port" "NodeId"
--        serviceContexTChan <- writeTChan serviceContexTChan serviceRequest

--        async (handle serviceContexTChan connection Idle
--                                (InitServiceNegotiationEvent serviceRequest))




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
