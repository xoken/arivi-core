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
import           Arivi.P2P
import           Arivi.Network
import qualified Arivi.P2P.Config                       as Config
import           Arivi.P2P.P2PEnv as PE
import           Arivi.P2P.ServiceRegistry
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.PubSub.Types

import           Control.Concurrent                     (threadDelay)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Lazy                   as BSL (ByteString)
import           Data.ByteString.Lazy.Char8             as BSLC (pack)
import qualified Data.HashMap.Strict                    as HM
import           Data.Monoid                            ((<>))
import           Data.String.Conv
import           Data.Text
import           System.Directory                       (doesPathExist)
import           System.Environment                     (getArgs)

type AppM = ReaderT (P2PEnv ServiceResource ServiceTopic String String) (LoggingT IO)

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

instance HasPRT AppM where
    getPeerReputationHistoryTableTVar = asks (tvPeerReputationHashTable . prtEnv)
    getServicesReputationHashMapTVar = asks (tvServicesReputationHashMap . prtEnv)
    getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv)
    getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv)
    getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv)

runAppM :: P2PEnv ServiceResource ServiceTopic String String-> AppM a -> LoggingT IO a
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
                20
                5
                3
                5
    Config.makeConfig config (path <> "/config.yaml")

runNode :: String -> IO ()
runNode configPath = do
    config <- Config.readConfig configPath
    env <- mkP2PEnv config (ResourceHandler globalHandlerRpc) globalHandlerPubSub [HelloWorld] [HelloWorldHeader]
    runFileLoggingT (toS $ Config.logFile config) $
        runAppM
            env
            (do initP2P config
                liftIO $ threadDelay 5000000
                liftIO $ putStrLn "Publish (y/n)?"
                answer <- liftIO getLine
                if answer == "y"
                    then stuffPublisher
                    else return ()
                -- getHelloWorld
                liftIO $ threadDelay 500000000)

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
