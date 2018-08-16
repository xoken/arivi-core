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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE GADTs, DataKinds    #-}

module Arivi.P2P.Kademlia.MessageHandler
    ( kademliaMessageHandler
    , kademliaHandlerHelper
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.Kademlia.Types
import           Arivi.P2P.Kademlia.VerifyPeer
import           Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Control.Concurrent.Async.Lifted       (async)
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Logger
import qualified Data.Text                             as T
import           Control.Monad.Except
import           Arivi.P2P.Types

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


kademliaHandlerHelper ::
       ( MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasP2PEnv m
       , HasLogging m
       )
    => Request 'Kademlia PayLoad
    -> m (Response 'Kademlia PayLoad)
kademliaHandlerHelper (KademliaRequest payload) = KademliaResponse <$> kademliaMessageHandler payload

kademliaMessageHandler ::
       ( MonadReader env m
       , HasNetworkConfig env NetworkConfig
       , HasP2PEnv m
       , HasLogging m
       )
    => PayLoad
    -> m PayLoad
kademliaMessageHandler payload = do
    let msgb = messageBody $ message payload
        rnep = fromEndPoint msgb
        rnid = Arivi.P2P.Kademlia.Types.nodeId msgb
        rpeer = Peer (rnid, rnep)
    nc@NetworkConfig {..} <- (^. networkConfig) <$> ask
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
            addToKBucket rpeer
                     -- Initiates the verification process
            _ <- async $ verifyPeer rpeer
                    -- liftIO $ do
                    --     print "Find_Node recieved and peer added"
                    --     i <- atomically $ H.size kb
                    --     print ("Kbucket size after mH " ++ show i)
            pl <- getKClosestPeersByNodeid rnid 20
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
                Right (KademliaResponse resp') -> do
                    case messageBody (message resp') of
                        FN_RESP _ pl _ -> return $ packVnR nc pl
                        _ -> throw KademliaInvalidResponse
        _ -> throw KademliaInvalidRequest
