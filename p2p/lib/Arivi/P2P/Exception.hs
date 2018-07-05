module Arivi.P2P.Exception
    ( AriviP2PException(..)
    ) where

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
    deriving (Show)

instance Exception AriviP2PException
