{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
    -> Int
    -> Int
    -> IO P2PEnv
makeP2Pinstance nodeid mIp tcpport udpport statsdIP statsdPort statsdPrefix sk sbound pingThreshold kademliaConcurrencyFactor = do
    newStatsdClient <- createStatsdClient statsdIP statsdPort statsdPrefix
    let netENV = mkAriviEnv (read $ show tcpport) (read $ show udpport) sk
        nc     = NetworkConfig nodeid mIp tcpport udpport
    -- TODO:  need to make port consistent
    p2p' <-
        makeP2PEnvironment
            nc
            sbound
            pingThreshold
            kademliaConcurrencyFactor
    let p2pEnv =
            p2p'
                { ariviNetworkEnv = netENV
                , _networkConfig = nc
                , statsdClient = newStatsdClient
                , tvarMessageTypeMap = insertHandlers
                }
    return p2pEnv

insertHandlers :: (HasP2PEnv m, HasLogging m) => MessageTypeMap m
insertHandlers =
    HM.insert
        Kademlia
        kademliaHandlerHelper
        (HM.insert Option optionsHandler (HM.insert RPC rpcHandler HM.empty))
