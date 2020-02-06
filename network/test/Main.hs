{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts     #-}
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
import           Arivi.Network.Types
import           Arivi.Utils.Logging
import           Control.Concurrent                     (MVar, newEmptyMVar,
                                                         putMVar, takeMVar,
                                                         threadDelay)
import           Control.Concurrent.Async               hiding
                                                         (mapConcurrently_)
import           Control.Concurrent.Async.Lifted        (mapConcurrently_)
import           Control.Exception
import           Control.Monad.Catch                    (MonadCatch)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control            (MonadBaseControl)
import qualified Data.ByteString                        as BS (ByteString)
import qualified Data.ByteString.Lazy                   as BSL (ByteString)
import qualified Data.ByteString.Lazy.Char8             as BSLC
import           Data.Time

import           Test.Hspec

type AppM = ReaderT AriviEnv (LoggingT IO)

instance HasNetworkEnv AppM where
    getEnv = ask

instance HasSecretKey AppM

runAppM :: AriviEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

generateString :: Int -> BSL.ByteString
generateString size = BSLC.pack (replicate size 'a')

sendManyConcurrent n cHandle str =
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

sender ::
       Int
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
            (do let nc =
                        NetworkConfig
                        { _nodeId = generateNodeId rk
                        , _ip = "127.0.0.1"
                        , _udpPort = read $ show udpPort
                        , _tcpPort = read $ show tcpPort
                        }
                handleOrFail <- openConnection nc UDP
                case handleOrFail of
                    Left e -> throw e
                    Right cHandle -> do
                        sendToHandle cHandle f
                        close cHandle
                liftIO $ print "done")

receiver ::
       Int
    -> Int
    -> SecretKey
    -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
    -> IO ()
receiver tcpPort udpPort sk handler = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (runUdpServer (show $ ariviEnvUdpPort env) (\_ _ h -> handler h))

senderTCP ::
       Int
    -> Int
    -> SecretKey
    -> SecretKey
    -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
    -> IO ()
senderTCP tcpPort udpPort sk rk f = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (do let nc =
                        NetworkConfig
                        { _nodeId = generateNodeId rk
                        , _ip = "127.0.0.1"
                        , _udpPort = read $ show udpPort
                        , _tcpPort = read $ show tcpPort
                        }
                handleOrFail <- openConnection nc TCP
                case handleOrFail of
                    Left e -> throw e
                    Right cHandle -> do
                        sendToHandle cHandle f
                        close cHandle
                liftIO $ print "done")

receiverTCP ::
       Int
    -> Int
    -> SecretKey
    -> (ConnectionHandle -> ReaderT AriviEnv (LoggingT IO) ())
    -> IO ()
receiverTCP tcpPort udpPort sk handler = do
    let env = mkAriviEnv tcpPort udpPort sk
    runStdoutLoggingT $
        runAppM
            env
            (runTcpServer (show $ ariviEnvTcpPort env) (\_ _ h -> handler h))

--initiator :: IO ()
initiator f = do
    let sender_sk = ACUPS.getSecretKey "ssssssssssssssssssssssssssssssss"
        recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    sender 8083 8083 sender_sk recv_sk f
    return ()

initiatorTCP f = do
    let sender_sk = ACUPS.getSecretKey "ssssssssssssssssssssssssssssssss"
        recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    senderTCP 8083 8083 sender_sk recv_sk f
    return ()

recipient :: (Int -> Int -> SecretKey -> t -> IO ()) -> t -> IO ()
recipient f handler = do
    let recv_sk = ACUPS.getSecretKey "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    f 8083 8083 recv_sk handler

myAmazingHandler ::
       (HasLogging m, HasSecretKey m)
    => BS.ByteString
    -> TransportType
    -> ConnectionHandle
    -> m ()
myAmazingHandler _ _ h = forever $ recv h

serverRecvHandler ::
       (MonadCatch m, MonadLogger m, MonadIO m, MonadBaseControl IO m)
    => MVar BSL.ByteString
    -> Int
    -> ConnectionHandle
    -> m ()
serverRecvHandler mvar n h = do
    r <- BSLC.concat <$> replicateM n (recv h)
    liftIO $ putMVar mvar r

sendAndRecvString f str n = do
    mvar <- newEmptyMVar
    server <- async (recipient receiver (serverRecvHandler mvar n))
    threadDelay 1000000
    initiator (`f` str)
    r <- takeMVar mvar
    r `shouldBe` BSLC.concat (replicate n str)
    cancel server

sendAndRecvStringTCP f str n = do
    mvar <- newEmptyMVar
    server <- async (recipient receiverTCP (serverRecvHandler mvar n))
    threadDelay 1000000
    initiatorTCP (`f` str)
    r <- takeMVar mvar
    r `shouldBe` BSLC.concat (replicate n str)
    cancel server

main :: IO ()
main =
    hspec $ do
        describe "UDP send and recv" $ do
            it "sends and recvs a single char" $ sendAndRecvString send "a" 1
            it "sends and recvs a small string" $
                sendAndRecvString send "hello" 1
            it "sends and recvs a 4K string" $
                sendAndRecvString send (generateString 4096) 1
        --it "sends and recvs a > 4K string" $
            --sendAndRecvString send (generateString 4097) 1
            it "sends and recvs 2 small strings serially" $
                sendAndRecvString (sendManySerial 2) "hello" 2
            it "sends and recvs 2 4K strings serially" $
                sendAndRecvString (sendManySerial 2) (generateString 4096) 2
            it "sends and recvs 2 small strings concurrently" $
                sendAndRecvString (sendManyConcurrent 2) "hello" 2
            it "sends and recvs 2 4k strings concurrently" $
                sendAndRecvString (sendManyConcurrent 2) (generateString 4096) 2
            it "sends and recvs 10 small strings serially" $
                sendAndRecvString (sendManySerial 100) "hello" 100
            it "sends and recvs 10 4K strings serially" $
                sendAndRecvString (sendManySerial 10) (generateString 4096) 10
            it "sends and recvs 10 small strings concurrently" $
                sendAndRecvString (sendManyConcurrent 10) "hello" 10
            it "sends and recvs 10 4k strings concurrently" $
                sendAndRecvString
                    (sendManyConcurrent 10)
                    (generateString 4096)
                    10
        describe "TCP send and recv" $ do
            it "sends and recvs a single char" $ sendAndRecvStringTCP send "a" 1
            it "sends and recvs a small string" $
                sendAndRecvStringTCP send "hello" 1
            it "sends and recvs a 4K string" $
                sendAndRecvStringTCP send (generateString 4096) 1
            it "sends and recvs a > 4K string" $
                sendAndRecvStringTCP send (generateString 4097) 1
            it "sends and recvs 2 small strings serially" $
                sendAndRecvStringTCP (sendManySerial 2) "hello" 2
            it "sends and recvs 2 4K strings serially" $
                sendAndRecvStringTCP (sendManySerial 2) (generateString 4096) 2
            it "sends and recvs 2 large strings serially" $
                sendAndRecvStringTCP (sendManySerial 2) (generateString 4097) 2
            it "sends and recvs 2 small strings concurrently" $
                sendAndRecvStringTCP (sendManyConcurrent 2) "hello" 2
            it "sends and recvs 2 4k strings concurrently" $
                sendAndRecvStringTCP
                    (sendManyConcurrent 2)
                    (generateString 4096)
                    2
            it "sends and recvs 2 large strings concurrently" $
                sendAndRecvStringTCP
                    (sendManyConcurrent 2)
                    (generateString 4097)
                    2
            it "sends and recvs 100 small strings serially" $
                sendAndRecvStringTCP (sendManySerial 100) "hello" 100
            it "sends and recvs 100 4K strings serially" $
                sendAndRecvStringTCP
                    (sendManySerial 100)
                    (generateString 4096)
                    100
            it "sends and recvs 100 large strings serially" $
                sendAndRecvStringTCP
                    (sendManySerial 100)
                    (generateString 4096)
                    100
            it "sends and recvs 100 small strings concurrently" $
                sendAndRecvStringTCP (sendManyConcurrent 100) "hello" 100
            it "sends and recvs 100 4k strings concurrently" $
                sendAndRecvStringTCP
                    (sendManyConcurrent 100)
                    (generateString 4096)
                    100
            it "sends and recvs 100 large strings concurrently" $
                sendAndRecvStringTCP
                    (sendManyConcurrent 100)
                    (generateString 4097)
                    100
