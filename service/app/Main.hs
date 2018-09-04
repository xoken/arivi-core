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

import           Service.HelloWorld

import           Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Network
import           Arivi.P2P.P2PEnv as PE
import           Arivi.P2P.ServiceRegistry
import           Arivi.P2P.Types
import           Arivi.P2P.Handler  (newIncomingConnectionHandler)
import           Arivi.P2P.Kademlia.LoadDefaultPeers
import           Arivi.P2P.MessageHandler.HandlerTypes

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async.Lifted        (async, wait)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified CreateConfig                           as Config
import           Data.ByteString.Lazy                   as BSL (ByteString)
import           Data.ByteString.Lazy.Char8             as BSLC (pack)
import           Data.Monoid                            ((<>))
import           Data.String.Conv
import           Data.Text
import           System.Directory                       (doesPathExist)
import           System.Environment                     (getArgs)

type AppM = ReaderT (P2PEnv ServiceResource String) (LoggingT IO)

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

instance HasNetworkConfig (P2PEnv r msg) NetworkConfig where
    networkConfig f p2p =
        fmap
             (\nc ->
                  p2p
                   { nodeEndpointEnv =
                         (nodeEndpointEnv p2p) {PE._networkConfig = nc}
                   })
            (f ((PE._networkConfig . nodeEndpointEnv) p2p))

instance HasArchivedResourcers AppM ServiceResource String where
    archived = asks (tvarArchivedResourceToPeerMap . rpcEnv)

instance HasTransientResourcers AppM ServiceResource String where
    transient = asks (tvarDynamicResourceToPeerMap . rpcEnv)

runAppM :: P2PEnv ServiceResource String -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

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
    Config.makeConfig config (path <> "/config.yaml")

runNode :: String -> IO ()
runNode configPath = do
    config <- Config.readConfig configPath
    let ha = Config.myIp config
    env <-
        makeP2Pinstance
            (generateNodeId (Config.secretKey config))
            ha
            (Config.tcpPort config)
            (Config.udpPort config)
            "127.0.0.1"
            8089
            "Xoken"
            (Config.secretKey config)
            20
            5
            3
    runFileLoggingT (toS $ Config.logFile config) $
        runAppM
            env
            (do tid' <-
                    async
                        (runUdpServer
                             (show (Config.udpPort config))
                             newIncomingConnectionHandler)
                tid <-
                    async
                        (runTcpServer
                             (show (Config.tcpPort config))
                             newIncomingConnectionHandler)
                liftIO $ threadDelay 1000000
                void $ async (loadDefaultPeers (Config.trustedPeers config))
                liftIO $ threadDelay 5000000
                registerHelloWorld
                liftIO $ threadDelay 3000000
                getHelloWorld
                liftIO $ threadDelay 3000000
                getHelloWorld
                wait tid
                wait tid')

main :: IO ()
main = do
    (path:_) <- getArgs
    b <- doesPathExist (path <> "/config.yaml")
    unless b (defaultConfig path)
    runNode (path <> "/config.yaml")

a :: Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')

myAmazingHandler :: (HasLogging m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h >>= send h
