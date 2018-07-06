{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( module Main
    ) where

import Arivi.Crypto.Utils.Keys.Signature
import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Env
import Arivi.Utils.Logging
import Arivi.Network

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (mapConcurrently_)
import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.ByteString      as BS  (ByteString)
import Data.ByteString.Lazy.Char8 as BSLC (pack)
import Data.Time
import System.Environment (getArgs)

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = ask

instance HasSecretKey AppM

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

sender :: Int -> Int -> SecretKey -> SecretKey -> Int -> Int -> IO ()
sender tcpPort udpPort sk rk n size = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (do let ha = "127.0.0.1"
                handleOrFail <- openConnection ha 8083 UDP (generateNodeId rk)
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

receiver :: Int -> Int -> SecretKey -> IO ()
receiver tcpPort udpPort sk = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM env (runUdpServer (show $ ariviEnvUdpPort env) myAmazingHandler)

sender' :: Int -> Int -> SecretKey -> SecretKey -> Int -> Int -> IO ()
sender' tcpPort udpPort sk rk n size = do
    let env = mkAriviEnv tcpPort udpPort sk
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

receiver' :: Int -> Int -> SecretKey -> IO ()
receiver' tcpPort udpPort sk = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM env (runTcpServer (show $ ariviEnvTcpPort env) myAmazingHandler)

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
main = do
    [size, n] <- getArgs
    _ <-
        recipient receiver `concurrently`
        (threadDelay 1000000 >> initiator sender size n) `concurrently`
        recipient receiver' `concurrently`
        (threadDelay 1000000 >> initiator sender' size n)
    return ()

a :: Int -> BSL.ByteString
a n = BSLC.pack (replicate n 'a')

myAmazingHandler :: (HasLogging m, HasSecretKey m) => BS.ByteString -> TransportType -> ConnectionHandle -> m ()
myAmazingHandler _ _ h = forever $ recv h
