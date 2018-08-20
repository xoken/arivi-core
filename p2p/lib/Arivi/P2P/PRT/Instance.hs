--------------------------------------------------------------------------------
-- |
-- Module      : Arivi.P2P.PRT.Types
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different functions that are used in the Peer
-- Reputation management
--
--------------------------------------------------------------------------------
module Arivi.P2P.PRT.Instance
    ( loadConfigFile
    , loadPRTConfigToHashMap
    , getReputationForServices
    , getReputationForP2P
    , updatePeerReputationHistory
    , updatePeerReputationForP2P
    , updatePeerReputationForServices
    ) where

import qualified Arivi.Network.Types         as Network (NodeId)
import           Arivi.P2P.P2PEnv            (HasP2PEnv (..))
import           Arivi.P2P.PRT.Exceptions    (PRTExecption (..))
import           Arivi.P2P.PRT.Types         (Config (..), PeerDeed (..),
                                              PeerReputationHistory (..),
                                              Reputation)
import           Control.Concurrent.STM.TVar (readTVarIO, writeTVar)
import           Control.Exception           (throw)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import qualified Data.HashMap.Strict         as HM (insert, lookup)
import           Data.Yaml                   (ParseException, decodeFileEither)

-- | Reads the config file and converts it's fields to config data type
loadConfigFile :: FilePath -> IO Config
loadConfigFile filePath = do
    mConfig <- decodeFileEither filePath :: IO (Either ParseException Config)
    case mConfig of
        Left e       -> throw e
        Right config -> return config

-- | This  function loads the fields of config file into respective HashMap
loadPRTConfigToHashMap :: (HasP2PEnv m) => m ()
loadPRTConfigToHashMap = do
    mConfig <- liftIO $ loadConfigFile "PRTConfig.yaml"
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    liftIO $
        atomically $ writeTVar servicesReputationHashMapTVar (services mConfig)
    liftIO $ atomically $ writeTVar p2pReputationHashMapTVar (p2p mConfig)
    return ()

-- | Gives the `Reputation` of given `PeerDeed` in case of P2P
getReputationForP2P :: (HasP2PEnv m) => PeerDeed -> m (Maybe Reputation)
getReputationForP2P peerDeed = do
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    p2pReputationHashMap <- liftIO $ readTVarIO p2pReputationHashMapTVar
    return $ HM.lookup peerDeed p2pReputationHashMap

-- | Gives the `Reputation` of given `PeerDeed` in case of Services
getReputationForServices :: (HasP2PEnv m) => String -> m (Maybe Reputation)
getReputationForServices peerDeed = do
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    servicesReputationHashMap <-
        liftIO $ readTVarIO servicesReputationHashMapTVar
    return $ HM.lookup peerDeed servicesReputationHashMap

-- | Updates the Peer Reputation History of given Peer's NodeId
updatePeerReputationHistory ::
       (HasP2PEnv m) => Network.NodeId -> Reputation -> m ()
updatePeerReputationHistory peerNodeId reputationToAward = do
    oldMapOfAllPeersHistoryTVar <- getPeerReputationHistoryTableTVar
    oldMapOfAllPeersHistory <- liftIO $ readTVarIO oldMapOfAllPeersHistoryTVar
    updatedPeerHistoryTable <-
        case HM.lookup peerNodeId oldMapOfAllPeersHistory of
            Just peerHistoryTable ->
                return
                    PeerReputationHistory
                    { nodeId = peerNodeId
                    , nofDeeds = nofDeeds peerHistoryTable + 1
                    , reputation =
                          reputation peerHistoryTable + reputationToAward
                    }
            Nothing ->
                return
                    PeerReputationHistory
                    { nodeId = peerNodeId
                    , nofDeeds = 1
                    , reputation = reputationToAward
                    }
    let newMapOfAllPeersHistory =
            HM.insert peerNodeId updatedPeerHistoryTable oldMapOfAllPeersHistory
    liftIO $
        atomically $
        writeTVar oldMapOfAllPeersHistoryTVar newMapOfAllPeersHistory
    return ()

-- | Updates the `Reputation` of given `PeerDeed` in case of P2P
updatePeerReputationForP2P ::
       (HasP2PEnv m) => Network.NodeId -> PeerDeed -> m ()
updatePeerReputationForP2P peerNodeId peerDeed = do
    maybeReputation <- getReputationForP2P peerDeed
    case maybeReputation of
        Just mReputation -> updatePeerReputationHistory peerNodeId mReputation
        Nothing -> throw PeerDeedNotFound

-- | Updates the `Reputation` of given `PeerDeed` in case of Services
updatePeerReputationForServices ::
       (HasP2PEnv m) => Network.NodeId -> String -> m ()
updatePeerReputationForServices peerNodeId peerDeed = do
    maybeReputation <- getReputationForServices peerDeed
    case maybeReputation of
        Just mReputation -> updatePeerReputationHistory peerNodeId mReputation
        Nothing -> throw PeerDeedNotFound
