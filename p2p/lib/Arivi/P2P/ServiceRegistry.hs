module Arivi.P2P.ServiceRegistry
    ( mkP2PEnv
    ) where

import           Arivi.Env
import qualified Arivi.P2P.Config as Config
import           Arivi.P2P.Kademlia.MessageHandler
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Handler
import           Arivi.P2P.Types
import           Arivi.Utils.Statsd

mkHandlers :: Handlers
mkHandlers = Handlers rpcHandler kademliaMessageHandler optionsHandler


mkP2PEnv :: Config.Config -> IO (P2PEnv r msg)
mkP2PEnv config = do
    let nc = NetworkConfig (Config.myNodeId config) (Config.myIp config) (Config.tcpPort config) (Config.udpPort config)
    let networkEnv = mkAriviEnv (read $ show $ Config.tcpPort config) (read $ show $ Config.udpPort config) (Config.secretKey config)
    P2PEnv <$> mkNodeEndpoint nc mkHandlers networkEnv <*> mkRpcEnv <*> mkKademlia nc (Config.sbound config) (Config.pingThreshold config) (Config.kademliaConcurrencyFactor config) <*> createStatsdClient "statsdIP" 8080 "statsdPrefix" <*> mkPRTEnv
