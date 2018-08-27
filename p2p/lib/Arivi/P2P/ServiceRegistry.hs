{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables       #-}
{-# LANGUAGE DuplicateRecordFields, KindSignatures, DataKinds, GADTs #-}

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
import           Codec.Serialise
import           Data.HashMap.Strict                   as HM

import           Control.Monad.Reader

makeP2Pinstance :: forall r . 
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
    -> IO (P2PEnv r)
makeP2Pinstance nodeid mIp tcpport udpport statsdIP statsdPort statsdPrefix sk sbound pingThreshold kademliaConcurrencyFactor = do
    newStatsdClient <- createStatsdClient statsdIP statsdPort statsdPrefix
    let netENV = mkAriviEnv (read $ show tcpport) (read $ show udpport) sk
        nc     = NetworkConfig nodeid mIp tcpport udpport
    -- TODO:  need to make port consistent
    nmap <- newTVarIO HM.empty
    let nodeEndpointEnv = mkNodeEndpoint nc nmap handlers netENV
    p2p' <-
        makeP2PEnvironment
            nc
            sbound
            pingThreshold
            kademliaConcurrencyFactor
    let p2pEnv =
            p2p'
                { statsdClient = newStatsdClient
                , nodeEndpointEnv = nodeEndpointEnv
                }
    return p2pEnv



handlers :: Handlers
handlers = Handlers rpcHandlerHelper kademliaHandlerHelper

{-
data Handler m = forall t msg. Handler (Request t msg -> m (Response t msg))

kH :: (MonadReader env m, HasP2PEnv m, HasNetworkConfig env NetworkConfig, HasLogging m) => Handler m
kH = Handler kademliaHandlerHelper

rH :: ( MonadReader env m
      , HasP2PEnv m
      , HasNetworkConfig env NetworkConfig
      , HasLogging m
      )
   => Handler m
rH = Handler rpcHandlerHelper

insertHandlers' ::
       forall m . (HasP2PEnv m, HasLogging m)
    => HashMap MessageType (Handler m)
insertHandlers' =
    HM.insert
        Kademlia
        (Handler kademliaHandlerHelper)
        (HM.insert RPC (Handler rpcHandlerHelper) HM.empty)
-}
