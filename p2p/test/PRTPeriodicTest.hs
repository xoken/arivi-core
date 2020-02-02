{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
    ( module Main
    ) where

import qualified Arivi.Crypto.Utils.Keys.Signature as S
import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import qualified Arivi.Crypto.Utils.PublicKey.Utils as U
import Arivi.Env
import Arivi.Network
import Arivi.P2P.Kademlia.LoadDefaultPeers (loadDefaultPeers)
import qualified Arivi.P2P.Kademlia.Types as T
import qualified Arivi.P2P.Kademlia.Utils as U
import Arivi.P2P.P2PEnv
import Arivi.P2P.ServiceRegistry
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL (ByteString)
import Data.ByteString.Lazy.Char8 as BSLC (pack)
import System.Environment (getArgs)

-- import           Arivi.P2P.MessageHandler.Handler       (newIncomingConnection)
import Arivi.P2P.PRT.Instance
import Arivi.P2P.PRT.Types
import Control.Monad.IO.Class (liftIO)
import qualified CreateConfig as Config
import Data.Monoid ((<>))
import Data.String.Conv
import Data.Text
import System.Directory (doesPathExist)

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
    -- getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getMessageTypeMapP2PEnv = tvarMessageTypeMap <$> getP2PEnv
    getWatcherTableP2PEnv = tvarWatchersTable <$> getP2PEnv
    getNotifiersTableP2PEnv = tvarNotifiersTable <$> getP2PEnv
    getTopicHandlerMapP2PEnv = tvarTopicHandlerMap <$> getP2PEnv
    getMessageHashMapP2PEnv = tvarMessageHashMap <$> getP2PEnv
    -- getKademliaConcurrencyFactor = kademliaConcurrencyFactor <$> getP2PEnv
    getArchivedResourceToPeerMapP2PEnv = tvarArchivedResourceToPeerMap <$> getP2PEnv
    getTransientResourceToPeerMap = tvarDynamicResourceToPeerMap <$> getP2PEnv
    -- getSelfNodeId = selfNId <$> getP2PEnv
    getPeerReputationHistoryTableTVar = tvPeerReputationHashTable <$> getP2PEnv
    getP2PReputationHashMapTVar = tvP2PReputationHashMap <$> getP2PEnv
    getServicesReputationHashMapTVar = tvServicesReputationHashMap <$> getP2PEnv
    getReputedVsOtherTVar = tvReputedVsOther <$> getP2PEnv
    getKClosestVsRandomTVar = tvKClosestVsRandom <$> getP2PEnv

runAppM :: P2PEnv -> AppM a -> LoggingT IO a
runAppM = flip runReaderT

getNodeId :: IO T.NodeId
getNodeId = do
    (sk, pk) <- S.generateKeyPair
    let pk = U.generateNodeId sk
    let nid = pk :: T.NodeId
    return nid

defaultConfig :: FilePath -> IO ()
defaultConfig path = do
    (sk, _) <- ACUPS.generateKeyPair
    let config = Config.Config 5678 5678 sk [] (generateNodeId sk) "127.0.0.1" (Data.Text.pack (path <> "/node.log"))
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
            8125
            "Xoken"
            (Config.secretKey config)
            3
            5
            6
    n1 <- getNodeId
    n2 <- getNodeId
    n3 <- getNodeId
    -- _ <- loadPeerReputationHistoryTable
    runFileLoggingT (toS $ Config.logFile config) $
        runAppM
            env
            (do _ <- loadPRTConfigToHashMap
                _ <- async $ savePRTHMtoDBPeriodically 1000
                loadPeerReputationHistoryTable
                p <- getReputationForP2P SignatureMismatch
                liftIO $ print p
                r <- getReputationForServices "ValidNotification"
                liftIO $ print r
                r1 <- getReputation n1
                liftIO $ print r1
                updatePeerReputationForP2P n1 SignatureMismatch
                r2 <- getReputation n1
                liftIO $ print r2
                r3 <- getReputation n2
                liftIO $ print r3
                updatePeerReputationForServices n2 "ValidNotification"
                r4 <- getReputation n2
                liftIO $ print r4
                loadPeerReputationHistoryTable
                return ())

-- runBSNode :: String -> IO ()
-- runBSNode configPath = do
--     config <- Config.readConfig configPath
--     let ha = "127.0.0.1"
--     env <-
--         makeP2Pinstance
--             (generateNodeId (Config.secretKey config))
--             ha
--             (Config.tcpPort config)
--             (Config.udpPort config)
--             "127.0.0.1"
--             8125
--             "Xoken"
--             (Config.secretKey config)
--             3
--     runFileLoggingT (toS $ Config.logFile config) $
--         runAppM
--             env
--             (runUdpServer (show (Config.tcpPort config)) newIncomingConnection)
main :: IO ()
main
    -- (path:_) <- getArgs
 = do
    let path = "./"
    b <- doesPathExist (path <> "/config.yaml")
    unless b (defaultConfig path)
    runNode (path <> "/config.yaml")

a :: Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')

myAmazingHandler :: (HasLogging m, HasSecretKey m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h >>= send h
