{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Arivi.Crypto.Utils.Keys.Signature
import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Env
import Arivi.Network
import Arivi.P2P.P2PEnv
import Arivi.P2P.ServiceRegistry
import Arivi.Utils.Logging
import Arivi.Utils.Statsd
import Service.HelloWorld

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (async, mapConcurrently_, wait)

import Control.Concurrent.Async.Lifted (async, mapConcurrently_, wait)
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL (ByteString)
import Data.ByteString.Lazy.Char8 as BSLC (pack)
import Data.Time
import System.Environment (getArgs)

import Arivi.P2P.Kademlia.LoadDefaultPeers (loadDefaultPeers)
import Arivi.P2P.Kademlia.Types (NodeEndPoint(..), NodeId, Peer(..))
import Arivi.P2P.MessageHandler.Handler (newIncomingConnection)
import qualified CreateConfig as Config
import Data.Monoid ((<>))
import Data.String.Conv
import Data.Text
import Network.Socket (PortNumber(..))
import System.Directory (doesPathExist, withCurrentDirectory)

type AppM = ReaderT P2PEnv (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = asks ariviNetworkEnv

instance HasSecretKey AppM

instance HasKbucket AppM where
    getKb = asks kbucket

instance HasStatsdClient AppM where
    getStatsdClient = asks statsdClient

instance HasP2PEnv AppM where
    getP2PEnv = ask
    getSelfNodeId = selfNId <$> getP2PEnv
    getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getMessageTypeMapP2PEnv = tvarMessageTypeMap <$> getP2PEnv
    getWatcherTableP2PEnv = tvarWatchersTable <$> getP2PEnv
    getNotifiersTableP2PEnv = tvarNotifiersTable <$> getP2PEnv
    getTopicHandlerMapP2PEnv = tvarTopicHandlerMap <$> getP2PEnv
    getMessageHashMapP2PEnv = tvarMessageHashMap <$> getP2PEnv
    getKademliaConcurrencyFactor = kademliaConcurrencyFactor <$> getP2PEnv
    getArchivedResourceToPeerMapP2PEnv =
        tvarArchivedResourceToPeerMap <$> getP2PEnv
    getTransientResourceToPeerMap = tvarDynamicResourceToPeerMap <$> getP2PEnv

runAppM :: P2PEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

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
            "89.98.98.98"
            8089
            "ad"
            (Config.secretKey config)
            3
    runFileLoggingT (toS $ Config.logFile config) $
        runAppM
            env
            (do tid' <-
                    async
                        (runUdpServer
                             (show (Config.udpPort config))
                             newIncomingConnection)
                tid <-
                    async
                        (runTcpServer
                             (show (Config.tcpPort config))
                             newIncomingConnection)
                liftIO $ threadDelay 1000000
                async (loadDefaultPeers (Config.trustedPeers config))
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
    (path:args) <- getArgs
    b <- doesPathExist (path <> "/config.yaml")
    unless b (defaultConfig path)
    runNode (path <> "/config.yaml")

a :: Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')

myAmazingHandler :: (HasLogging m, HasSecretKey m,HasStatsdClient m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h >>= send h
