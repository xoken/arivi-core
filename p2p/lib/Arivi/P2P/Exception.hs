module Arivi.P2P.Exception
    ( AriviP2PException(..)
    ) where

import           Arivi.Network       (AriviNetworkException)
import           Arivi.P2P.RPC.Types (MessageTypeRPC)
import           Control.Exception

data AriviP2PException
    = KademliaKbIndexDoesNotExist
    | KademliaInvalidPeer
    | KademliaDefaultPeerDoesNotExists
    | HandlerSendMessageTimeout
    | HandlerOpenConnectionError
    | HandlerNotRequest
    | HandlerUuidNotFound
    | HandlerConnectionBroken
    | P2PDeserialisationException
    | HandlerConnectionDetailsNotFound
    | HandlerNetworkException AriviNetworkException
    | KademliaInvalidRequest
    | KademliaInvalidResponse
    | RPCResourceNotFoundException
    | RPCHandlerResourceNotFoundException
    | RPCEmptyNodeListException
    | RPCInvalidMessageType MessageTypeRPC
    | SendOptionsFailedException
    | OptionsInvalidMessageType MessageTypeRPC
    | OptionsHandlerInvalidMessageType MessageTypeRPC
    | KademliaDeserialiseFailure
    | PubSubInvalidResponseException
    | PubSubNoWatcherOrNotifierException
    | KademliaNoVerifiedPeer
    deriving (Show)

instance Exception AriviP2PException
