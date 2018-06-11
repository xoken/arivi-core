-- |
-- Module      :  Arivi.P2P.MessageHandler
-- Copyright   :
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
--

module Arivi.P2P.MessageHandler
(
    sendOptionMessage
    , getUUID
    , formServiceMessage
    , selfNodeId
    , checkUUID
)

where

import           Data.ByteString.Char8       as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy        as ByteStringLazy (toStrict)
import qualified Data.Map                    as Map
import           Data.Maybe                  ()
import           Data.Time.Clock
import           Data.UUID                   as UUID
import           Data.UUID.V4

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               ()

import           Codec.Serialise             (serialise)

import           Arivi.Network.Connection    ()
import           Arivi.Network.Types         (ConnectionId, NodeId,
                                              TransportType (..))
import           Arivi.P2P.Types
--readSupportedMessage :: P2PMessage -> TVar ResourceList -> ()
--readSupportedMessage p2pMessage resourceListTVar = do
 --   return ()
    --do
        --check uuid exists in a table containing uuid of options sent
        --if not there then discard and kick peer ending all connections with him
        --else process it
    --let
        --peerID = from p2pMessage
        --peer = findPeer peerID -- find peer from peer table if needed, might not be needed to
        --
        --message = fromJust p2pType p2pMessage --P2PMessage might change
    --addToResourceListTVar (resourceList message) peer resourceListTVar
    --similar func call to add peer to TopicToPeer Map


sendOptionMessage :: [Peer] -> TVar UUIDMap -> IO ()
sendOptionMessage [] _ = return ()
sendOptionMessage (peer:peerList) uuidMapTVar =
    do
        uuid <- getUUID
        time <- getCurrentTime
        atomically( modifyTVar' uuidMapTVar (Map.insert uuid (peer, time)))
        let message = formOptionMessage (peerNodeId peer) uuid
            connId = openConnection (peerNodeId peer) (peerIp peer) (peerPort peer) (peerTransportType peer)
        sendMessage connId (ByteStringLazy.toStrict $ serialise message)






{-forms the option message-}
formOptionMessage :: NodeId -> P2PUUID -> P2PMessage
formOptionMessage node uuid1 =
    P2PMessage {
        uuid = uuid1,
        to = node,
        from = selfNodeId,
        responseCode = 1,
        p2pType =  Just Options
    }

{- forms the get or return type messsages from the service layer-}
formServiceMessage :: NodeId -> NodeId -> ResourceID -> P2PUUID -> String -> Bool -> P2PMessage
formServiceMessage sender receiver resourceID uuid1 message flag =
    P2PMessage {
        uuid = uuid1,
        to = receiver,
        from = sender,
        responseCode = getResponseCodeFromFlag flag,
        p2pType =  Just $ returnMessageType flag message resourceID
    }
{-returns the type based on the flag-}
returnMessageType :: Bool -> String -> ResourceID -> MessageType
returnMessageType flag resourceID message =
    if flag then
        Return{resource = resourceID, serviceMessage = message} else
        Get{resource = resourceID, serviceMessage = message}

selfNodeId :: NodeId
selfNodeId = pack "12334556"

checkUUID ::P2PUUID -> Bool
checkUUID uuid = True

getUUID :: IO String
getUUID = UUID.toString <$> nextRandom
{-adds particular response for get or ret to be decided on response codes later-}

getResponseCodeFromFlag :: Bool -> ResponseCode
getResponseCodeFromFlag flag = 1



openConnection :: NodeId -> IP -> Port -> TransportType -> ConnectionId
openConnection nodeId ip port transportType = pack "892sadasd346384"

sendMessage :: ConnectionId -> Char8.ByteString -> IO ()
sendMessage connectionId byteString = return ()

getNewConnection :: ConnectionId
getNewConnection = pack "892sadasd346384"

kademilaGetPeer :: Int -> [Peer]
kademilaGetPeer count = [Peer { peerNodeId = pack "892346384" , peerIp = "127.0.0.1", peerPort = 300, peerTransportType = TCP},
                            Peer { peerNodeId = pack "892346384" , peerIp = "127.0.0.1", peerPort = 300, peerTransportType = TCP}]

{-forms the supported message-}
formSupportedMessage :: NodeId  --  ^
                     -> P2PUUID
                     -> Maybe MessageType
                     -> ResponseCode
                     -> P2PMessage
formSupportedMessage node uuid1 rpctype responsecode =
    P2PMessage {
        uuid = uuid1,
        to = node,
        from = selfNodeId,
        responseCode = responsecode,
        p2pType = rpctype
    }
 {--}
