module Arivi.P2P.Exception
    ( AriviP2PException(..)
    ) where

<<<<<<< HEAD
import           Arivi.Network     (AriviNetworkException)
import           Control.Exception
=======
import Arivi.Network (AriviNetworkException)
import Control.Exception
>>>>>>> breaking out arivi-core from arivi

data AriviP2PException
    = KademliaKbIndexDoesNotExist
    | KademliaInvalidPeer
    | KademliaDefaultPeerDoesNotExists
    | SendMessageTimeout
    | HandlerOpenConnectionError
    | HandlerNotRequest
    | InvalidUuid
    | HandlerConnectionBroken
    | DeserialiseFailureP2P
    | PeerNotFound
    | NetworkException AriviNetworkException
    | KademliaInvalidRequest
    | KademliaInvalidResponse
    | RPCResourceNotFoundException
    | RPCHandlerResourceNotFoundException
    | RPCEmptyNodeListException
    | RPCInvalidMessageType
    | SendOptionsFailedException
    | OptionsInvalidMessageType
    | OptionsHandlerInvalidMessageType
    | KademliaDeserialiseFailure
    | PubSubInvalidResponseException
    | PubSubNoWatcherOrNotifierException
    | KademliaNoVerifiedPeer
    | KademliaPeerDoesNotExist
    deriving (Eq, Ord, Show)

instance Exception AriviP2PException
