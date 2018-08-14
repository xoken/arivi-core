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
    | RPCInvalidMessageType MessageTypeRPC
    | SendOptionsFailedException
    | OptionsInvalidMessageType MessageTypeRPC
    | OptionsHandlerInvalidMessageType MessageTypeRPC
    | KademliaDeserialiseFailure
    | KademliaNoVerifiedPeer
    deriving (Show)

instance Exception AriviP2PException
