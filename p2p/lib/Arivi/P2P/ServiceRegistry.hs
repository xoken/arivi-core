{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}

module Arivi.P2P.ServiceRegistry
    ( makeP2Pinstance
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Env
import           Arivi.P2P.Kademlia.MessageHandler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv

-- import           Arivi.P2P.RPC.Functions
-- import           Arivi.P2P.RPC.SendOptions
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import           Arivi.Utils.Statsd
import           Codec.Serialise
import           Control.Concurrent.STM
import           Data.HashMap.Strict                   as HM

import           Control.Monad.Reader

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
        nc = NetworkConfig nodeid mIp tcpport udpport
    -- TODO:  need to make port consistent
    p2p' <- makeP2PEnvironment nc sbound pingThreshold kademliaConcurrencyFactor
    let p2pEnv =
            p2p'
            { ariviNetworkEnv = netENV
            , _networkConfig = nc
            , statsdClient = newStatsdClient
                -- , tvarMessageTypeMap = handlers
            }
    return p2pEnv
-- handlers :: Handlers
-- handlers = Handler rpcHandler kademliaHandler
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
