{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE RankNTypes #-}

module Arivi.P2P.ServiceRegistry
    ( makeP2Pinstance
    ) where

import           Arivi.P2P.Kademlia.Kbucket
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Functions
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.Types
import           Arivi.Utils.Statsd

import           Data.HashMap.Strict                   as HM

import           Control.Concurrent
import           Control.Concurrent.STM
import           Network.Socket

makeP2Pinstance ::
       NodeId
    -> IP
    -> PortNumber
    -> PortNumber
    -> IP
    -> PortNumber
    -> String
    -> IO ()
makeP2Pinstance nodeid ip tcpport udpport statsdIP statsdPort statsdPrefix = do
    ariviP2PInstanceTvar <-
        atomically (newTVar (AriviP2PInstance nodeid ip tcpport udpport))
    -- newKBucket <- createKbucket nodeid ip tcpport udpport
    newStatsdClient <- createStatsdClient statsdIP statsdPort statsdPrefix
    p2p' <- makeP2PEnvironment
    -- let newmap = HM.insert RPC rpcHandler $ tvarMessageTypeMap p2p'
    let p2pEnv =
            p2p'
                { tvarAriviP2PInstance = ariviP2PInstanceTvar
                        -- , kbucket = newKBucket
                , statsdClient = newStatsdClient
                , tvarMessageTypeMap = insertHandlers
                }
    return ()

--     liftIO $
--         forkIO $
--         runP2Papp
--             p2pEnv
-- --add funcs to run at start
--             ()
insertHandlers ::
       forall m. (HasP2PEnv m)
    => MessageTypeMap m
insertHandlers =
    HM.insert Option optionsHandler (HM.insert RPC rpcHandler HM.empty)
