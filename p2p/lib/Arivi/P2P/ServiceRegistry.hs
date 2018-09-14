module Arivi.P2P.ServiceRegistry
    ( mkP2PEnv
    ) where

import           Arivi.Env
import qualified Arivi.P2P.Config as Config
import           Arivi.P2P.PubSub.Handler
import           Arivi.P2P.PubSub.Env
import           Arivi.P2P.Kademlia.MessageHandler
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Handler
import           Arivi.P2P.Types
import           Arivi.Utils.Statsd

mkHandlers :: Handlers
mkHandlers = Handlers rpcHandler kademliaMessageHandler optionsHandler pubSubHandler


mkP2PEnv :: Config.Config -> IO (P2PEnv r t rmsg pmsg)
mkP2PEnv config = do
    let nc = NetworkConfig (Config.myNodeId config) (Config.myIp config) (Config.tcpPort config) (Config.udpPort config)
    let networkEnv = mkAriviEnv (read $ show $ Config.tcpPort config) (read $ show $ Config.udpPort config) (Config.secretKey config)
    P2PEnv <$> mkNodeEndpoint nc mkHandlers networkEnv <*> mkRpcEnv <*> mkPubSub <*> mkKademlia nc (Config.sbound config) (Config.pingThreshold config) (Config.kademliaConcurrencyFactor config) <*> createStatsdClient "127.0.0.1" 8080 "statsdPrefix" <*> mkPRTEnv

-- makeP2Pinstance ::
--        NodeId
--     -> IP
--     -> Port
--     -> Port
--     -> IP
--     -> Port
--     -> String
--     -> SecretKey
--     -> Int
--     -> Int
--     -> Int
--     -> IO (P2PEnv r t rmsg pmsg)
-- makeP2Pinstance nodeid mIp tcpport udpport statsdIP statsdPort statsdPrefix sk sbound pingThreshold kademliaConcurrencyFactor = do
--     let netENV = mkAriviEnv (read $ show tcpport) (read $ show udpport) sk
--         nc = NetworkConfig nodeid mIp tcpport udpport
--     -- TODO:  need to make port consistent
--     P2PEnv <$> mkNodeEndpoint nc mkHandlers netENV
--            <*> mkRpcEnv
--            <*> mkPubSub
--            <*> mkKademlia nc sbound pingThreshold kademliaConcurrencyFactor
--            <*> createStatsdClient statsdIP statsdPort statsdPrefix
