{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Arivi.P2P.P2PEnv
    ( module Arivi.P2P.P2PEnv
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.Types

import qualified Arivi.P2P.Kademlia.Types              as T
import           Arivi.Utils.Statsd
import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Concurrent.STM.TQueue
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader                  (ReaderT, ask, asks,
                                                        runReaderT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Data.HashMap.Strict                   as HM
import qualified STMContainers.Map                     as H

-- | Peer information encapsulated in a single structure
newtype Peer = Peer
    { getPeer :: (T.NodeId, T.NodeEndPoint)
    } deriving (Show)

instance Eq Peer where
    Peer (x, _) == Peer (a, _) = a == x

-- | K-bucket to store peers
newtype Kbucket k v = Kbucket
    { getKbucket :: H.Map k v
    }

class (MonadIO m, MonadBaseControl IO m) =>
      HasKbucket m
    where
    getKb :: m (Kbucket Int [Peer])

data P2PEnv = P2PEnv
    { tvarAriviP2PInstance  :: TVar AriviP2PInstance
    , tvarNodeIdPeerMap     :: TVar NodeIdPeerMap
    , tqueueKadem           :: TQueue MessageInfo
    , tqueueRPC             :: TQueue MessageInfo
    , tqueuePubSub          :: TQueue MessageInfo
    , tqueueOption          :: TQueue MessageInfo
    , tvarResourceToPeerMap :: TVar ResourceToPeerMap
    , kbucket               :: Kbucket Int [Peer]
    , statsdClient          :: StatsdClient
    }

type P2Papp = ReaderT P2PEnv IO

class (MonadIO m, MonadBaseControl IO m, HasKbucket m, HasStatsdClient m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getAriviTVarP2PEnv :: m (TVar AriviP2PInstance)
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getkademTQueueP2PEnv :: m (TQueue MessageInfo)
    getrpcTQueueP2PEnv :: m (TQueue MessageInfo)
    getpubsubTQueueP2PEnv :: m (TQueue MessageInfo)
    getoptionTQueueP2PEnv :: m (TQueue MessageInfo)
    getResourceToPeerMapP2PEnv :: m (TVar ResourceToPeerMap)

instance HasKbucket P2Papp where
    getKb = asks kbucket

instance HasStatsdClient P2Papp where
    getStatsdClient = asks statsdClient

instance HasP2PEnv P2Papp where
    getP2PEnv = ask
    getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getkademTQueueP2PEnv = tqueueKadem <$> getP2PEnv
    getrpcTQueueP2PEnv = tqueueRPC <$> getP2PEnv
    getpubsubTQueueP2PEnv = tqueuePubSub <$> getP2PEnv
    getoptionTQueueP2PEnv = tqueueOption <$> getP2PEnv
    getResourceToPeerMapP2PEnv = tvarResourceToPeerMap <$> getP2PEnv

runP2Papp :: P2PEnv -> P2Papp a -> IO a
runP2Papp = flip runReaderT

makeP2PEnvironment :: IO P2PEnv
makeP2PEnvironment = do
    nmap <- newTVarIO HM.empty
    kqueue <- newTQueueIO
    rqueue <- newTQueueIO
    pqueue <- newTQueueIO
    oqueue <- newTQueueIO
    r2pmap <- newTVarIO HM.empty
    return
        P2PEnv
            { tvarNodeIdPeerMap = nmap
            , tqueueKadem = kqueue
            , tqueueRPC = rqueue
            , tqueuePubSub = pqueue
            , tqueueOption = oqueue
            , tvarResourceToPeerMap = r2pmap
            }
