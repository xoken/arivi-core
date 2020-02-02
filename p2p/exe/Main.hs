{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main
    ( module Main
    ) where

import           Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Network
import qualified Arivi.P2P.Config                       as Config
import           Arivi.P2P.P2PEnv as PE
import           Arivi.P2P.ServiceRegistry
import           Arivi.P2P.Types
import           Arivi.P2P.Handler  (newIncomingConnectionHandler)
import           Arivi.P2P.Kademlia.LoadDefaultPeers
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Env
import           Arivi.P2P.PubSub.Class

import           Control.Concurrent.Async.Lifted        (async, wait)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Lazy                   as BSL (ByteString)
import           Data.ByteString.Lazy.Char8             as BSLC (pack)
import           Data.Monoid                            ((<>))
import           Data.String.Conv
import           Data.Text
import           System.Directory                       (doesPathExist)
import           System.Environment                     (getArgs)

type AppM = ReaderT (P2PEnv ByteString ByteString ByteString ByteString) (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = asks (ariviNetworkEnv . nodeEndpointEnv)

instance HasSecretKey AppM

instance HasKbucket AppM where
    getKb = asks (kbucket . kademliaEnv)

instance HasStatsdClient AppM where
    getStatsdClient = asks statsdClient

instance HasNodeEndpoint AppM where
    getEndpointEnv = asks nodeEndpointEnv
    getNetworkConfig = asks (PE._networkConfig . nodeEndpointEnv)
    getHandlers = asks (handlers . nodeEndpointEnv)
    getNodeIdPeerMapTVarP2PEnv = asks (tvarNodeIdPeerMap . nodeEndpointEnv)

instance HasNetworkConfig (P2PEnv r t rmsg pmsg) NetworkConfig where
    networkConfig f p2p =
        fmap
            (\nc ->
                 p2p
                 { nodeEndpointEnv =
                       (nodeEndpointEnv p2p) {PE._networkConfig = nc}
                 })
            (f ((PE._networkConfig . nodeEndpointEnv) p2p))

instance HasArchivedResourcers AppM ByteString ByteString where
    archived = asks (tvarArchivedResourceToPeerMap . rpcEnv)

instance HasTransientResourcers AppM ByteString ByteString where
    transient = asks (tvarDynamicResourceToPeerMap . rpcEnv)


instance HasPRT AppM where
    getPeerReputationHistoryTableTVar = asks (tvPeerReputationHashTable . prtEnv)
    getServicesReputationHashMapTVar = asks (tvServicesReputationHashMap . prtEnv)
    getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv)
    getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv)
    getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv)

instance HasTopics (P2PEnv r t rmsg pmsg) t where
    topics = pubSubTopics . psEnv
instance HasSubscribers (P2PEnv r t rmsg pmsg) t where
    subscribers = pubSubSubscribers . psEnv
instance HasNotifiers (P2PEnv r t rmsg pmsg) t where
    notifiers = pubSubNotifiers . psEnv
instance HasInbox (P2PEnv r t rmsg pmsg) pmsg where
    inbox = pubSubInbox . psEnv
instance HasCache (P2PEnv r t rmsg pmsg) pmsg where
    cache = pubSubCache . psEnv
instance HasTopicHandlers (P2PEnv r t rmsg pmsg) t pmsg where
    topicHandlers = pubSubHandlers . psEnv
instance HasPubSubEnv (P2PEnv r t rmsg pmsg) t pmsg where
    pubSubEnv = psEnv

runAppM :: P2PEnv ByteString ByteString ByteString ByteString-> AppM a -> LoggingT IO a
runAppM = flip runReaderT

{--
writeConfigs path = do
    (skBootstrap, _) <- ACUPS.generateKeyPair
    (skNode1, _) <- ACUPS.generateKeyPair
    (skNode2, _) <- ACUPS.generateKeyPair
    let bootstrapPort = 8080
        bootstrapConfig = Config.Config bootstrapPort bootstrapPort skBootstrap [] (generateNodeId skBootstrap) (Data.Text.pack path <> "/bootstrapNode.log")
        config1 = Config.Config 8081 8081 skNode1 [Peer (generateNodeId skBootstrap,
NodeEndPoint "127.0.0.1" bootstrapPort bootstrapPort)] (generateNodeId skNode1) (Data.Text.pack path <> "/node1.log")
        config2 = Config.Config 8082 8082 skNode2 [Peer (generateNodeId skBootstrap,
NodeEndPoint "127.0.0.1" bootstrapPort bootstrapPort)] (generateNodeId skNode2) (Data.Text.pack path <> "/node2.log")
    Config.makeConfig bootstrapConfig (path <> "/bootstrapConfig.yaml")
    Config.makeConfig config1 (path <> "/config1.yaml")
    Config.makeConfig config2 (path <> "/config2.yaml")
-}
defaultConfig :: FilePath -> IO ()
defaultConfig path = do
    (sk, _) <- ACUPS.generateKeyPair
    let config =
            Config.Config
                5678
                5678
                sk
                []
                (generateNodeId sk)
                "127.0.0.1"
                (Data.Text.pack (path <> "/node.log"))
                20
                5
                3
    Config.makeConfig config (path <> "/config.yaml")

runNode :: String -> IO ()
runNode configPath = do
    print "reading config..."
    config <- Config.readConfig configPath
    env <- mkP2PEnv config
    runFileLoggingT (toS $ Config.logFile config) $
    -- runStdoutLoggingT $
        runAppM
            env
            -- (runTcpServer (show (Config.tcpPort config))  newIncomingConnection)
            (do tid <-
                    async
                        (runUdpServer
                             (show (Config.udpPort config))
                             newIncomingConnectionHandler)
                loadDefaultPeers (Config.trustedPeers config)
                wait tid
            -- let (bsNodeId, bsNodeEndPoint) = getPeer $ Prelude.head (Config.trustedPeers config)
            -- handleOrFail <- openConnection (nodeIp bsNodeEndPoint) (tcpPort bsNodeEndPoint) TCP bsNodeId
            -- case handleOrFail of
            --     Left e -> throw e
            --     Right cHandle -> do
            --         time <- liftIO getCurrentTime
            --         liftIO $ print timep
            --         mapConcurrently_
            --             (const (send cHandle (a 1024)))
            --             [1 .. 10]
            --         forever $ recv cHandle
            --         time2 <- liftIO getCurrentTime
            --         liftIO $ print time2
            -- liftIO $ print "done"
             )

runBSNode :: String -> IO ()
runBSNode configPath = do
    config <- Config.readConfig configPath
    env <- mkP2PEnv config
    runFileLoggingT (toS $ Config.logFile config) $
    -- runStdoutLoggingT $
        runAppM
            env
            (runUdpServer (show (Config.tcpPort config)) newIncomingConnectionHandler
            --async (runTcpServer (show (Config.udpPort config)) newIncomingConnection)
            -- return ()
             )

main :: IO ()
main = do
    (path:_) <- getArgs
    b <- doesPathExist (path <> "/config.yaml")
    unless b (defaultConfig path)
    runNode (path <> "/config.yaml")
    --threadDelay 5000000
    --async (runNode (path <> "/config1.yaml"))
    --threadDelay 5000000
    --async (runNode (path <> "/config2.yaml"))
    --threadDelay 100000000
    --return ()

-- main' = do
--     [size, n] <- getArgs
--     _ <-
--         recipient receiver `concurrently`
--         (threadDelay 1000000 >> initiator sender size n)
--     return ()
a :: Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')

myAmazingHandler :: (HasLogging m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h >>= send h
