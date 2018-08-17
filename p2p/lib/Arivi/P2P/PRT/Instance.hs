module Arivi.P2P.PRT.Instance
    ( loadConfigFile
        -- , updatePeerKarma
    , loadPRTConfigToHashMap
    ) where

import qualified Arivi.Network.Types         as Network (NodeId)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PRT.Exceptions    (PRTExecption (..))
import           Arivi.P2P.PRT.Types         (Config (..), PeerDeed (..),
                                              PeerReputationHistory (..),
                                              Reputation)
import           Control.Concurrent.STM      (TVar, newTVarIO)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM
import qualified Data.HashMap.Strict         as HM
import           Data.Yaml

-- | Reads the config file and converts it's fields to config data type
loadConfigFile :: FilePath -> IO Config
loadConfigFile filePath = do
    mConfig <- decodeFileEither filePath :: IO (Either ParseException Config)
    case mConfig of
        Left e       -> throw e
        Right config -> return config

loadPRTConfigToHashMap :: (HasP2PEnv m) => m ()
loadPRTConfigToHashMap = do
    mConfig <- liftIO $ loadConfigFile "PRTConfig.yaml"
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    liftIO $
        atomically $ writeTVar servicesReputationHashMapTVar (services mConfig)
    liftIO $ atomically $ writeTVar p2pReputationHashMapTVar (p2p mConfig)
    return ()

getReputationForServices :: (HasP2PEnv m) => String -> m (Maybe Reputation)
getReputationForServices peerDeed = do
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    servicesReputationHashMap <-
        liftIO $ readTVarIO servicesReputationHashMapTVar
    return $ HM.lookup peerDeed servicesReputationHashMap

getReputationForP2P :: (HasP2PEnv m) => PeerDeed -> m (Maybe Reputation)
getReputationForP2P peerDeed = do
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    p2pReputationHashMap <- liftIO $ readTVarIO p2pReputationHashMapTVar
    return $ HM.lookup peerDeed p2pReputationHashMap

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

--
updatePeerReputationForServices ::
       (HasP2PEnv m) => Network.NodeId -> String -> m ()
updatePeerReputationForServices peerNodeId peerDeed = do
    maybeReputation <- getReputationForServices peerDeed
    case maybeReputation of
        Just reputation -> updatePeerReputationHistory peerNodeId reputation
        Nothing         -> throw PeerDeedNotFound

updatePeerReputationForP2P ::
       (HasP2PEnv m) => Network.NodeId -> PeerDeed -> m ()
updatePeerReputationForP2P peerNodeId peerDeed = do
    maybeReputation <- getReputationForP2P peerDeed
    case maybeReputation of
        Just reputation -> updatePeerReputationHistory peerNodeId reputation
        Nothing         -> throw PeerDeedNotFound
