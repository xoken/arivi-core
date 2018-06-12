{-# LANGUAGE FlexibleContexts,     MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances     #-}

module Arivi.P2P.P2PEnv
(
    module Arivi.P2P.P2PEnv
)
where

import           Arivi.P2P.Types
import           Data.Map                       as Map
import           Control.Concurrent.STM         (TVar,newTVarIO)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Control.Monad.Reader           (ReaderT, ask, runReaderT)

data P2PEnv = P2PEnv {
    tvarAriviP2PInstance     :: TVar AriviP2PInstance
  , tvarWatchersMap          :: TVar WatchersMap
  , tvarSubscriptionMap      :: TVar SubscriptionMap
  , tvarPeerToTopicMap       :: TVar PeerToTopicMap
  , tvarTopicContextMap      :: TVar TopicContextMap
  , tvarTopicToServiceMap    :: TVar TopicToServiceMap
  , minPeerCountPerTopic :: MinPeerCountPerTopic
}

type P2Papp = ReaderT P2PEnv IO

class (MonadIO m, MonadBaseControl IO m) => HasP2PEnv m where
  getP2PEnv                     :: m P2PEnv
  getAriviTVarP2PEnv            :: m (TVar AriviP2PInstance)
  getWatchersMapTVarP2PEnv      :: m (TVar WatchersMap)
  getSubscriptionMapP2PEnv      :: m (TVar SubscriptionMap)
  getPeerToTopicMapP2PEnv       :: m (TVar PeerToTopicMap)
  getTopicContextMapP2PEnv      :: m (TVar TopicContextMap)
  getTopicToServiceMapP2PEnv    :: m (TVar TopicToServiceMap)
  getMinPeerCountPerTopicP2PEnv :: m  MinPeerCountPerTopic

instance HasP2PEnv P2Papp where
  getP2PEnv                  = ask
  getAriviTVarP2PEnv         = tvarAriviP2PInstance <$> getP2PEnv
  getWatchersMapTVarP2PEnv   = tvarWatchersMap <$> getP2PEnv
  getSubscriptionMapP2PEnv   = tvarSubscriptionMap <$> getP2PEnv
  getPeerToTopicMapP2PEnv    = tvarPeerToTopicMap <$> getP2PEnv
  getTopicContextMapP2PEnv   = tvarTopicContextMap <$> getP2PEnv
  getTopicToServiceMapP2PEnv = tvarTopicToServiceMap <$> getP2PEnv
  getMinPeerCountPerTopicP2PEnv = minPeerCountPerTopic <$> getP2PEnv

runP2Papp :: P2PEnv -> P2Papp a -> IO a
runP2Papp = flip runReaderT

makeP2PEnvironment :: IO P2PEnv
makeP2PEnvironment = do

    wmap   <- newTVarIO  Map.empty
    smap   <- newTVarIO  Map.empty
    pttmap <- newTVarIO  Map.empty
    tcmap  <- newTVarIO  Map.empty
    ttsmap <- newTVarIO  Map.empty

    return P2PEnv {   tvarWatchersMap       = wmap
                    , tvarSubscriptionMap   = smap
                    , tvarPeerToTopicMap    = pttmap
                    , tvarTopicContextMap   = tcmap
                    , tvarTopicToServiceMap = ttsmap
                }
