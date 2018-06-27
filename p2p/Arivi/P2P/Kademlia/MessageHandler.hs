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
module Arivi.P2P.MessageHandler
    ( messageHandler
    ) where

import           Arivi.P2P.Kademlia.Kbucket
import qualified Arivi.P2P.Kademlia.Types       as T
import           Arivi.P2P.Kademlia.XorDistance
import           Arivi.P2P.P2PEnv
import           Arivi.Utils.Exception
import           Codec.Serialise                (serialise)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM              (atomicallly)
import           P2P.Types

messageHandler ::
       (HasP2PEnv m) => T.PayLoad -> m (Either AriviException T.PayLoad)
messageHandler payl = do
    let msg = deserialise payl
        msgt = messageType $ message payl
        msgb = messageBody $ message payl
        nep = T.fromEndPoint msgb
        rnid = nodeId msgb
    kb <- getKb
    p2pInstanceTVar <- getAriviTVarP2PEnv
    p2pInstance <- atomically $ readTVar p2pInstanceTVar
    let lnid = selfNodeId p2pInstance
    case msgb of
        (T.Ping x) ->
            return $
            serialise $ T.packPong lnid (nodeIp nep) (udpPort nep) (tcpPort nep)
        (T.FN_NODE y) -> do
            pl <- getKClosestPeersByNodeid rnid
            return $
                serialise $
                T.packFnR lnid pl (nodeIp nep) (udpPort nep) (tcpPort nep)
