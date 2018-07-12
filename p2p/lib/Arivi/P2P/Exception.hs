module Arivi.P2P.Exception
    ( AriviP2PException(..)
    ) where

import           Arivi.P2P.RPC.Types (MessageTypeRPC)
import           Control.Exception

data AriviP2PException
    = KademliaKbIndexDoesNotExist
    | KademliaInvalidPeer
    | KademliaDefaultPeerDoesNotExists
    | HandlerSendMessageTimeout
    | HandlerOpenConnectionError
    | HandlerNotRequest
    | HandlerConnectionBroken
    | KademliaInvalidRequest
    | KademliaInvalidResponse
    | RPCResourceNotFoundException
    | RPCHandlerResourceNotFoundException
    | RPCEmptyNodeListException
    | RPCInvalidMessageType { mTypeRPC :: MessageTypeRPC }
    | SendOptionsFailedException
    | OptionsInvalidMessageType { mTypeOpt :: MessageTypeRPC }
    | OptionsHandlerInvalidMessageType { mTypeOptH :: MessageTypeRPC }
    deriving (Show)

instance Exception AriviP2PException
