-- |
-- Module      : Arivi.Kademlia.MessageHandler
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This module process the incoming kademlia request and produces the sutiable
-- response as per the Kademlia protocol.
--
module Arivi.P2P.Kademlia.MessageHandler
    ( kademliaMessageHandler
    ) where

import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Exception
import           Codec.Serialise             (deserialise, serialise)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.ByteString.Lazy        as L

kademliaMessageHandler ::
       (HasP2PEnv m) => L.ByteString -> m (Either AriviException L.ByteString)
kademliaMessageHandler payl = do
    let payl' = deserialise payl :: PayLoad
        msgb = messageBody $ message payl'
        nep = fromEndPoint msgb
        rnid = nodeId msgb
    p2pInstanceTVar <- getAriviTVarP2PEnv
    p2pInstance <- liftIO $ atomically $ readTVar p2pInstanceTVar
    let lnid = selfNodeId p2pInstance
    case msgb of
        PING {} ->
            return $
            Right $
            serialise $ packPong lnid (nodeIp nep) (udpPort nep) (tcpPort nep)
        FIND_NODE {} -> do
            pl <- getKClosestPeersByNodeid rnid 5
            case pl of
                Right pl2 ->
                    return $
                    Right $
                    serialise $
                    packFnR lnid pl2 (nodeIp nep) (udpPort nep) (tcpPort nep)
                Left _ -> return $ Left KademliaInvalidPeer
        _ -> return $ Left KademliaInvalidRequest
