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

import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Concurrent.Async hiding (mapConcurrently_)
import Control.Concurrent.Async.Lifted (mapConcurrently_)
import Control.Concurrent.STM.TQueue
import Control.Exception
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Data.ByteString      as BS  (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Time
import System.Environment (getArgs)

import Test.Hspec

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = ask

instance HasSecretKey AppM

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

generateString :: Int -> BSL.ByteString
generateString size = BSLC.pack (replicate size 'a')

sendManyConcurrent cHandle str n =
    let sendIt = send cHandle str
    in mapConcurrently_ (const sendIt) [1 .. n]

sendManySerial n cHandle str =
    let sendIt = send cHandle str
    in replicateM_ n sendIt

sendToHandle cHandle f = do
    time <- liftIO getCurrentTime
    liftIO $ print time
    f cHandle
    time2 <- liftIO getCurrentTime
    liftIO $ print time2

sender :: Int
       -> Int
       -> SecretKey
       -> SecretKey
       -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
       -> IO ()
sender tcpPort udpPort sk rk f = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (do let ha = "127.0.0.1"
                handleOrFail <- openConnection ha 8083 UDP (generateNodeId rk)
                case handleOrFail of
                    Left e -> throw e
                    Right cHandle -> do
                        sendToHandle cHandle f
                        close cHandle
                liftIO $ print "done")

receiver
    :: Int
    -> Int
    -> SecretKey
    -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
    -> IO ()
receiver tcpPort udpPort sk handler = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $ runAppM env $
        runUdpServer (show $ ariviEnvUdpPort env) (\_ _ h -> handler h)

sender'
    :: Int
    -> Int
    -> SecretKey
    -> SecretKey
    -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
    -> IO ()
sender' tcpPort udpPort sk rk f = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (do let ha = "127.0.0.1"
                handleOrFail <- openConnection ha 8083 TCP (generateNodeId rk)
                case handleOrFail of
                    Left e -> throw e
                    Right cHandle -> do
                        sendToHandle cHandle f
                        close cHandle
                liftIO $ print "done")

receiver' :: Int -> Int -> SecretKey -> IO ()
receiver' tcpPort udpPort sk = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM env (runTcpServer (show $ ariviEnvTcpPort env) myAmazingHandler)

--initiator :: IO ()
initiator f = do
    let sender_sk = ACUPS.getSecretKey "ssssssssssssssssssssssssssssssss"
        recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    sender 8083 8083 sender_sk recv_sk f
    return ()

recipient :: (Int -> Int -> SecretKey -> t -> IO ()) -> t -> IO ()
recipient f handler = do
    let recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    f 8083 8083 recv_sk handler

myAmazingHandler
    :: (HasLogging m, HasSecretKey m)
    => BS.ByteString -> TransportType -> ConnectionHandle -> m ()
myAmazingHandler _ _ h = forever $ recv h

serverRecvHandler
    :: (MonadCatch m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
    => MVar BSL.ByteString -> Int -> ConnectionHandle -> m ()
serverRecvHandler mvar n h = do
    replicateM_ n $ do
        r <- recv h
        liftIO $ putMVar mvar r

sendAndRecvString f str n = do
    mvar <- newEmptyMVar
    server <- async (recipient receiver (serverRecvHandler mvar n))
    threadDelay 1000000
    initiator (\h -> f h str)
    r <- replicateM n $ takeMVar mvar
    BSLC.concat r `shouldBe` BSLC.concat (replicate n str)
    cancel server

main :: IO ()
main = hspec $ do
    describe "UDP send and recv" $ do
        it "sends and recvs a single char" $ sendAndRecvString send "a" 1
        it "sends and recvs a small string" $ sendAndRecvString send "hello" 1
        it "sends and recvs a 4K string" $
            sendAndRecvString send (generateString 4096) 1
        it "sends and recvs a > 4K string" $
            sendAndRecvString send (generateString 4097) 1

        it "sends and recvs 2 small strings serially" $
            sendAndRecvString (sendManySerial 2) "hello" 2
        it "sends and recvs 2 4K strings serially" $
            sendAndRecvString (sendManySerial 2) (generateString 4096) 2
        it "sends and recvs 2 large strings serially" $
            sendAndRecvString (sendManySerial 2) (generateString 4097) 2

        it "sends and recvs 100 small strings serially" $
            sendAndRecvString (sendManySerial 100) "hello" 100
        it "sends and recvs 100 4K strings serially" $
            sendAndRecvString (sendManySerial 100) (generateString 4096) 100
        it "sends and recvs 100 large strings serially" $
            sendAndRecvString (sendManySerial 100) (generateString 4097) 100
