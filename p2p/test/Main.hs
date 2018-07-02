{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( module Main
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import           Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Env
import           Arivi.Network
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.ServiceRegistry
import           Arivi.Utils.Logging

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               hiding (async,
                                                         mapConcurrently_)

import           Control.Concurrent.Async.Lifted        (async,
                                                         mapConcurrently_)
import           Control.Concurrent.STM.TQueue
import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString.Lazy                   as BSL (ByteString)
import           Data.ByteString.Lazy.Char8             as BSLC (pack)
import           Data.Time
import           System.Environment                     (getArgs)

import           CreateConfig
import           Network.Socket                         (PortNumber (..))

type AppM = ReaderT P2PEnv (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = asks ariviNetworkEnv

instance HasSecretKey AppM

instance HasLogging AppM

instance HasKbucket AppM where
    getKb = asks kbucket

instance HasStatsdClient AppM where
    getStatsdClient = asks statsdClient

instance HasP2PEnv AppM where
    getP2PEnv = ask
    getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getkademTQueueP2PEnv = tqueueKadem <$> getP2PEnv
    getrpcTQueueP2PEnv = tqueueRPC <$> getP2PEnv
    getpubsubTQueueP2PEnv = tqueuePubSub <$> getP2PEnv
    getoptionTQueueP2PEnv = tqueueOption <$> getP2PEnv
    getResourceToPeerMapP2PEnv = tvarResourceToPeerMap <$> getP2PEnv
    getMessageTypeMapP2PEnv = tvarMessageTypeMap <$> getP2PEnv
    getWatcherTableP2PEnv = tvarWatchersTable <$> getP2PEnv
    getNotifiersTableP2PEnv = tvarNotifiersTable <$> getP2PEnv
    getTopicHandlerMapP2PEnv = tvarTopicHandlerMap <$> getP2PEnv
    getMessageHashMapP2PEnv = tvarMessageHashMap <$> getP2PEnv

runAppM :: P2PEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

nodeS :: PortNumber -> PortNumber -> SecretKey -> SecretKey -> IO ()
nodeS tcpPort udpPort sk rk = do
    let ha = "127.0.0.1"
    env <-
        makeP2Pinstance
            (generateNodeId sk)
            ha
            tcpPort
            udpPort
            "89.98.98.98"
            8089
            "ad"
            sk
    runStdoutLoggingT $
        runAppM
            env
            (do async (runTcpServer (show tcpPort) myAmazingHandler)
                async (runUdpServer (show udpPort) myAmazingHandler)
                return ())

sender ::
       PortNumber -> PortNumber -> SecretKey -> SecretKey -> Int -> Int -> IO ()
sender tcpPort udpPort sk rk n size = do
    let ha = "127.0.0.1"
    env <-
        makeP2Pinstance
            (generateNodeId sk)
            ha
            tcpPort
            udpPort
            "89.98.98.98"
            8089
            "ad"
            sk
    runStdoutLoggingT $
        runAppM
            env
            (do let ha = "127.0.0.1"
                handleOrFail <- openConnection ha 8083 TCP (generateNodeId rk)
                case handleOrFail of
                    Left e -> throw e
                    Right cHandle -> do
                        time <- liftIO getCurrentTime
                        liftIO $ print time
                        mapConcurrently_
                            (const (send cHandle (a size)))
                            [1 .. n]
                        time2 <- liftIO getCurrentTime
                        liftIO $ print time2
                liftIO $ print "done")

receiver :: PortNumber -> PortNumber -> SecretKey -> IO ()
receiver tcpPort udpPort sk = do
    let ha = "127.0.0.1"
    env <-
        makeP2Pinstance
            (generateNodeId sk)
            ha
            tcpPort
            udpPort
            "89.98.98.98"
            8089
            "ad"
            sk
    runStdoutLoggingT $
        runAppM env (runTcpServer (show tcpPort) myAmazingHandler)

--initiator :: IO ()
initiator f size n = do
    let sender_sk = ACUPS.getSecretKey "ssssssssssssssssssssssssssssssss"
        recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    _ <-
        threadDelay 1000000 >>
        f 8083 8083 sender_sk recv_sk (read n) (read size)
    threadDelay 1000000000000
    return ()

--recipient :: IO ()
recipient f = do
    let recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    f 8083 8083 recv_sk
    threadDelay 1000000000000

main :: IO ()
main
    -- [size, n] <- getArgs
    -- _ <-
    --     recipient receiver `concurrently`
    --     (threadDelay 1000000 >> initiator sender size n)
 = do
    makeConfig
        (PortNum 8083)
        (PortNum 8083)
        "path"
        "/home/pawan/xoken/arivi/config.yaml"
    readConfig "/home/pawan/xoken/arivi/config.yaml"
    return ()

a :: Int -> BSL.ByteString
a n = BSLC.pack (replicate n 'a')

myAmazingHandler :: (HasLogging m, HasSecretKey m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h
