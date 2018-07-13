{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Arivi.P2P.ServiceRegistry
    ( makeP2Pinstance
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Env
import           Arivi.P2P.Kademlia.MessageHandler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Functions
import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Control.Concurrent.STM
import           Data.HashMap.Strict                   as HM

makeP2Pinstance ::
       NodeId
    -> IP
    -> Port
    -> Port
    -> IP
    -> Port
    -> String
    -> SecretKey
    -> Int
    -> IO P2PEnv
makeP2Pinstance nodeid mIp tcpport udpport statsdIP statsdPort statsdPrefix sk alpha = do
    ariviP2PInstanceTvar <-
        atomically (newTVar (AriviP2PInstance nodeid mIp tcpport udpport))
    -- newKBucket <- createKbucket nodeid mIP tcpport udpport
    newStatsdClient <- createStatsdClient statsdIP statsdPort statsdPrefix
    let netENV = mkAriviEnv (read $ show tcpport) (read $ show udpport) sk -- TODO:  need to make port consistent
    p2p' <- makeP2PEnvironment mIp nodeid tcpport udpport alpha
    -- let newmap = HM.insert RPC rpcHandler $ tvarMessageTypeMap p2p'
    let p2pEnv =
            p2p'
                { ariviNetworkEnv = netENV
                , tvarAriviP2PInstance = ariviP2PInstanceTvar
                        -- , kbucket = newKBucket
                , statsdClient = newStatsdClient
                , tvarMessageTypeMap = insertHandlers
                }
    return p2pEnv

--     liftIO $
--         forkIO $
--         runP2Papp
--             p2pEnv
-- --add funcs to run at start
--             ()
insertHandlers :: (HasP2PEnv m, HasLogging m) => MessageTypeMap m
insertHandlers =
    HM.insert
        Kademlia
        kademliaMessageHandler
        (HM.insert Option optionsHandler (HM.insert RPC rpcHandler HM.empty))
