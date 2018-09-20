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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE DataKinds       #-}

module Arivi.P2P.Kademlia.MessageHandler
    ( kademliaMessageHandler
    ) where

import Arivi.P2P.Exception
import Arivi.P2P.Kademlia.Kbucket
import Arivi.P2P.Kademlia.Types
import Arivi.P2P.Kademlia.VerifyPeer
import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.Types
import Control.Concurrent.Async.Lifted (async)
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T

-- | Handler function to process incoming kademlia requests, requires a
--   P2P instance to get access to local node information and kbukcet itself.
--   It takes a bytesting as input which is deserialized to kademlia
--   payload, based on the type of request inside payload an appropriate
--   response is returned to the caller.
-- | As per Kademlia protocol there are two valid requests i.e PING and
--   FIND_NODE, in case of PING a simple PONG response is returned to let the
--   request initiator know that remote node is still active. In case of
--   FIND_NODE remote node asks for node closest to a given nodeId thus local
--   kbucket is queried to extract k-closest node known by the local node and a
--   list of k-closest peers wrapped in payload type is returned as a serialised
--   bytestring.

kademliaMessageHandler ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => Request 'Kademlia PayLoad
    -> m (Response 'Kademlia PayLoad)
kademliaMessageHandler (KademliaRequest payload) = KademliaResponse <$> do
    let msgb = messageBody $ message payload
        rnep = fromEndPoint msgb
        rnid = Arivi.P2P.Kademlia.Types.nodeId msgb
        rpeer = Peer (rnid, rnep)
    nc@NetworkConfig {..} <- asks (^. networkConfig)
    case msgb of
        PING {} -> do
            $(logDebug) $
                T.append
                    (T.pack "Ping Message Recieved from : ")
                    (T.pack (show rnep))
            return $ packPong nc
        FIND_NODE {} -> do
            $(logDebug) $
                T.append
                    (T.pack "Find_Node Message Recieved from : ")
                    (T.pack (show rnep))
            void . runExceptT $ addToKBucket rpeer
                     -- Initiates the verification process
            void . async $ runExceptT $ verifyPeer rpeer
                    -- liftIO $ do
                    --     print "Find_Node recieved and peer added"
                    --     i <- atomically $ H.size kb
                    --     print ("Kbucket size after mH " ++ show i)
            pl <- runExceptT $ getKClosestPeersByNodeid rnid 20
            case pl of
                Right pl2 -> return $ packFnR nc pl2
                Left _ -> throw KademliaInvalidPeer
        VERIFY_NODE _ tnid refnid tnep _ -> do
            $(logDebug) $
                T.append
                    (T.pack "Verify_Node Message Recieved from : ")
                    (T.pack (show rnep))
            let findNodeMsg = packFindMsg nc refnid
            resp <-
                runExceptT $
                issueKademliaRequest
                    (NetworkConfig
                         tnid
                         (nodeIp tnep)
                         (udpPort tnep)
                         (tcpPort tnep))
                    (KademliaRequest findNodeMsg)
            case resp of
                Left e -> throw e
                Right (KademliaResponse resp') ->
                    case messageBody (message resp') of
                        FN_RESP _ pl _ -> return $ packVnR nc pl
                        _ -> throw KademliaInvalidResponse
        _ -> throw KademliaInvalidRequest
