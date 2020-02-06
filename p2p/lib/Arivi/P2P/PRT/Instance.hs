{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
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
    , getReputation
    , getWeightages
    , getAllReputedNodes
    , getKNodes
    , savePRTHMtoDBPeriodically
    , loadPeerReputationHistoryTable
    ) where

import qualified Arivi.Network.Types as Network (NodeId)
import Arivi.P2P.Exception (AriviP2PException)
import Arivi.P2P.Kademlia.Kbucket
    ( Peer(..)
    , getDefaultNodeId
    , getKClosestPeersByNodeid
    , getKRandomPeers
    , getPeersByNodeIds
    )
import qualified Arivi.P2P.LevelDB as LevelDB (getValue, putValue)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PRT.Exceptions (PRTExecption(..))
import Arivi.P2P.PRT.Types (Config(..), PeerDeed(..), PeerReputationHistory(..), PeerReputationHistoryTable, Reputation)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar (readTVarIO, writeTVar)
import Control.Exception (throw)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.STM (atomically)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import qualified Data.HashMap.Strict as HM (fromList, insert, lookup, size, toList)
import Data.List (sortBy)
import Data.Ratio (Ratio, Rational, denominator, numerator)
import Data.Yaml (ParseException, decodeFileEither)

-- | Reads the config file and converts it's fields to config data type
loadConfigFile :: FilePath -> IO Config
loadConfigFile filePath = do
    mConfig <- decodeFileEither filePath :: IO (Either ParseException Config)
    case mConfig of
        Left e -> throw e
        Right config -> return config

isValidRatio :: Integral a => Ratio a -> Bool
isValidRatio mRatio = do
    let mNumerator = numerator mRatio
    let mDenominator = denominator mRatio
    let totalRatio = mNumerator + mDenominator
    let firstTerm = (mNumerator * 100) `div` totalRatio
    let secondTerm = (mDenominator * 100) `div` totalRatio
    case firstTerm + secondTerm of
        100 -> True
        _ -> False

-- | This  function loads the fields of config file into respective HashMap
loadPRTConfigToHashMap :: (HasPRT m, MonadIO m) => m ()
loadPRTConfigToHashMap = do
    mConfig <- liftIO $ loadConfigFile "PRTConfig.yaml"
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    reputedVsOtherTVar <- getReputedVsOtherTVar
    kClosestVsRandomTVar <- getKClosestVsRandomTVar
    liftIO $ atomically $ writeTVar servicesReputationHashMapTVar (services mConfig)
    liftIO $ atomically $ writeTVar p2pReputationHashMapTVar (p2p mConfig)
    if isValidRatio (reputedVsOther mConfig :: Rational)
        then liftIO $ atomically $ writeTVar reputedVsOtherTVar (reputedVsOther mConfig)
        else throw InvalidRatioReputedVsOther
    if isValidRatio (kClosestVsRandom mConfig :: Rational)
        then liftIO $ atomically $ writeTVar kClosestVsRandomTVar (kClosestVsRandom mConfig)
        else throw InvalidRatioKClosestVsRandom
    return ()

-- | Gives the `Reputation` of given `PeerDeed` in case of P2P
getReputationForP2P :: (HasPRT m, MonadIO m) => PeerDeed -> m (Maybe Reputation)
getReputationForP2P peerDeed = do
    p2pReputationHashMapTVar <- getP2PReputationHashMapTVar
    p2pReputationHashMap <- liftIO $ readTVarIO p2pReputationHashMapTVar
    return $ HM.lookup peerDeed p2pReputationHashMap

-- | Gives the `Reputation` of given `PeerDeed` in case of Services
getReputationForServices :: (HasPRT m, MonadIO m) => String -> m (Maybe Reputation)
getReputationForServices peerDeed = do
    servicesReputationHashMapTVar <- getServicesReputationHashMapTVar
    servicesReputationHashMap <- liftIO $ readTVarIO servicesReputationHashMapTVar
    return $ HM.lookup peerDeed servicesReputationHashMap

-- | Updates the Peer Reputation History of given Peer's NodeId
updatePeerReputationHistory :: (HasPRT m, MonadIO m) => Network.NodeId -> Reputation -> m ()
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
                        , reputation = reputation peerHistoryTable + reputationToAward
                        }
            Nothing -> return PeerReputationHistory {nodeId = peerNodeId, nofDeeds = 1, reputation = reputationToAward}
    let newMapOfAllPeersHistory = HM.insert peerNodeId updatedPeerHistoryTable oldMapOfAllPeersHistory
    liftIO $ atomically $ writeTVar oldMapOfAllPeersHistoryTVar newMapOfAllPeersHistory
    return ()

-- | Updates the `Reputation` of given `PeerDeed` in case of P2P
updatePeerReputationForP2P :: (HasPRT m, MonadIO m) => Network.NodeId -> PeerDeed -> m ()
updatePeerReputationForP2P peerNodeId peerDeed = do
    maybeReputation <- getReputationForP2P peerDeed
    case maybeReputation of
        Just mReputation -> updatePeerReputationHistory peerNodeId mReputation
        Nothing -> throw PeerDeedNotFound

-- | Updates the `Reputation` of given `PeerDeed` in case of Services
updatePeerReputationForServices :: (HasPRT m, MonadIO m) => Network.NodeId -> String -> m ()
updatePeerReputationForServices peerNodeId peerDeed = do
    maybeReputation <- getReputationForServices peerDeed
    case maybeReputation of
        Just mReputation -> updatePeerReputationHistory peerNodeId mReputation
        Nothing -> throw PeerDeedNotFound

-- | Gives the current reputation of Peer identified by given NodeId
getReputation :: (HasPRT m, MonadIO m) => Network.NodeId -> m (Maybe Reputation)
getReputation peerNodeId = do
    mapOfAllPeersHistoryTVar <- getPeerReputationHistoryTableTVar
    mapOfAllPeersHistory <- liftIO $ readTVarIO mapOfAllPeersHistoryTVar
    case HM.lookup peerNodeId mapOfAllPeersHistory of
        Just peerHistoryTable -> return $ Just $ reputation peerHistoryTable
        Nothing -> return Nothing

-- | Sums the denominator and numerator of the given Rational
getTotal :: Rational -> Integer
getTotal mRatio = numerator mRatio + denominator mRatio

-- | Gives the no of NonReputed Peers from given k based on the given config
getNoOfNonReputed :: Integer -> Rational -> Integer
getNoOfNonReputed k mReputedVsOther = k * denominator mReputedVsOther `div` getTotal mReputedVsOther

-- | Gives the no of Closest Peers from given k based on the given config
getnoOfClosest :: Integer -> Rational -> Integer
getnoOfClosest nonReputedNo mKClosestVsRandom =
    nonReputedNo * numerator mKClosestVsRandom `div` getTotal mKClosestVsRandom

-- | Gives the no of Random Peers from given k based on the given config
getnoOfRandom :: Integer -> Rational -> Integer
getnoOfRandom nonReputedNo mKClosestVsRandom =
    nonReputedNo * denominator mKClosestVsRandom `div` getTotal mKClosestVsRandom

-- | Given the total no of Peers this function splits it into Reputed,Closest
-- and Random based on the weightages defined in the config file
getWeightages :: (HasPRT m, MonadIO m) => Integer -> m (Integer, Integer, Integer)
getWeightages k = do
    reputedVsOtherTVar <- getReputedVsOtherTVar
    mReputedVsOther <- liftIO $ readTVarIO reputedVsOtherTVar
    kClosestVsRandomTVar <- getKClosestVsRandomTVar
    mKClosestVsRandom <- liftIO $ readTVarIO kClosestVsRandomTVar
    let noOfNonReputed = getNoOfNonReputed k mReputedVsOther
    let noOfClosest = getnoOfClosest noOfNonReputed mKClosestVsRandom
    let noOfRandom = getnoOfRandom noOfNonReputed mKClosestVsRandom
    let noOfReputed = k - (noOfClosest + noOfRandom)
    return (noOfReputed, noOfClosest, noOfRandom)

-- | Sorts the given Peer History details according to Reputation and their No
-- of deeds
-- sortGT ::
--        (Ord a1, Ord a2) => (a3, (a4, a2, a1)) -> (a5, (a6, a2, a1)) -> Ordering
sortGT :: (a1, PeerReputationHistory) -> (a2, PeerReputationHistory) -> Ordering
sortGT (_, PeerReputationHistory _ d1 r1) (_, PeerReputationHistory _ d2 r2)
    | r1 < r2 = GT
    | r1 > r2 = LT
    | r1 == r2 = compare d2 d1
sortGT (_, PeerReputationHistory {}) (_, PeerReputationHistory {}) = error "Something went wrong"

-- | Gives the list of NodeIds from given list of Peer History list
getNodeIds :: [(Network.NodeId, PeerReputationHistory)] -> [Network.NodeId]
getNodeIds [] = []
getNodeIds [(a, _)] = [a]
getNodeIds ((a, _):y) = a : getNodeIds y

-- | Gives given no of reputed Peers
getReputedNodes :: (HasKbucket m, MonadIO m) => Integer -> PeerReputationHistoryTable -> m [Peer]
getReputedNodes n mapOfAllPeersHistory = do
    let sortedListofAllPeersHistory = sortBy sortGT (HM.toList mapOfAllPeersHistory)
    liftIO $ print sortedListofAllPeersHistory
    eitherNReputedPeerList <-
        runExceptT $ getPeersByNodeIds (getNodeIds $ take (fromIntegral n) sortedListofAllPeersHistory)
    case eitherNReputedPeerList of
        Left e -> throw e
        Right nReputedPeerList -> return nReputedPeerList

-- | Gives list of all reputed Peer's NodeIds present in the
--   mapOfAllPeersHistory
getAllReputedNodes :: (HasPRT m, MonadIO m) => m [Network.NodeId]
getAllReputedNodes = do
    mapOfAllPeersHistoryTVar <- getPeerReputationHistoryTableTVar
    mapOfAllPeersHistory <- liftIO $ readTVarIO mapOfAllPeersHistoryTVar
    let sortedListofAllPeersHistory = sortBy sortGT (HM.toList mapOfAllPeersHistory)
    return $ getNodeIds sortedListofAllPeersHistory

-- | Gives K no of Peer's containting Reputed,Closest and Random based on the
-- weightages defined in the config file
getKNodes :: (HasKbucket m, HasPRT m, MonadIO m) => Integer -> ExceptT AriviP2PException m [Peer]
getKNodes k = do
    (noOfReputed, noOfClosest, noOfRandom) <- lift $ getWeightages k
    selfNodeId <- getDefaultNodeId
    mapOfAllPeersHistoryTVar <- lift getPeerReputationHistoryTableTVar
    mapOfAllPeersHistory <- (lift . liftIO) $ readTVarIO mapOfAllPeersHistoryTVar
    let availableReputedPeers = fromIntegral $ HM.size mapOfAllPeersHistory
    kRandomPeers <- lift $ getKRandomPeers (fromIntegral noOfRandom)
    if availableReputedPeers < noOfReputed
        then do
            let requiredClosestPeers = fromIntegral (noOfClosest + (noOfReputed - availableReputedPeers))
            closestPeers <- getKClosestPeersByNodeid selfNodeId requiredClosestPeers
            reputedPeers <- lift $ getReputedNodes availableReputedPeers mapOfAllPeersHistory
            return $ reputedPeers ++ closestPeers ++ kRandomPeers
        else do
            closestPeers <- getKClosestPeersByNodeid selfNodeId (fromIntegral noOfClosest)
            reputedPeers <- lift $ getReputedNodes noOfReputed mapOfAllPeersHistory
            return $ reputedPeers ++ closestPeers ++ kRandomPeers

-- | This function dumps PeerReputationHistoryTable to Level DB database
-- periodically
savePRTHMtoDBPeriodically :: (MonadUnliftIO m, HasPRT m) => Int -> m ()
savePRTHMtoDBPeriodically timeInterval = do
    mapOfAllPeersHistoryTVar <- getPeerReputationHistoryTableTVar
    mapOfAllPeersHistory <- liftIO $ readTVarIO mapOfAllPeersHistoryTVar
    let listofAllPeersHistory = HM.toList mapOfAllPeersHistory
    liftIO $ threadDelay timeInterval
    LevelDB.putValue "PeerReputationHistoryTable" (Char8.pack $ show listofAllPeersHistory)
    savePRTHMtoDBPeriodically timeInterval

-- | Loads the maybeMapOfAllPeersHistory from datbase to
--  mapOfAllPeersHistoryTVar of P2P Environment
loadPeerReputationHistoryTable :: (MonadUnliftIO m, HasPRT m) => m ()
loadPeerReputationHistoryTable = do
    mapOfAllPeersHistoryTVar <- getPeerReputationHistoryTableTVar
    maybeMapOfAllPeersHistory <- LevelDB.getValue "PeerReputationHistoryTable"
    case maybeMapOfAllPeersHistory of
        Nothing -> return ()
        Just mapPHty -> do
            let mapOfAllPeersHistory = read (Char8.unpack mapPHty) :: [(Network.NodeId, PeerReputationHistory)]
            liftIO $ atomically $ writeTVar mapOfAllPeersHistoryTVar (HM.fromList mapOfAllPeersHistory)
