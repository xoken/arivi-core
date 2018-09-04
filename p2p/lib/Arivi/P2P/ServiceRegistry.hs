module Arivi.P2P.ServiceRegistry
    ( makeP2Pinstance
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Env
import           Arivi.P2P.RPC.Handler
import           Arivi.P2P.Kademlia.MessageHandler
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.Utils.Statsd

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
    -> IO (P2PEnv r msg)
makeP2Pinstance nodeid mIp tcpport udpport statsdIP statsdPort statsdPrefix sk sbound pingThreshold kademliaConcurrencyFactor = do
    let netENV = mkAriviEnv (read $ show tcpport) (read $ show udpport) sk
        nc = NetworkConfig nodeid mIp tcpport udpport
    -- TODO:  need to make port consistent
    P2PEnv <$> mkNodeEndpoint nc mkHandlers netENV
           <*> mkRpcEnv
           <*> mkKademlia nc sbound pingThreshold kademliaConcurrencyFactor
           <*> createStatsdClient statsdIP statsdPort statsdPrefix

mkHandlers :: Handlers
mkHandlers = Handlers rpcHandler kademliaMessageHandler optionsHandler
