{-# LANGUAGE FlexibleContexts,     MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances     #-}

module Arivi.P2P.P2PEnv
(
    module Arivi.P2P.P2PEnv
)
where

import              Arivi.P2P.MessageHandler.HandlerTypes
import              Arivi.P2P.Types

import              Data.HashMap.Strict             as HM
import              Control.Concurrent.STM          (TVar,newTVarIO)
import              Control.Concurrent.STM.TQueue
import              Control.Monad.IO.Class          (MonadIO)
import              Control.Monad.Trans.Control     (MonadBaseControl)
import              Control.Monad.Reader            (ReaderT, ask, runReaderT)

data P2PEnv = P2PEnv {
    tvarAriviP2PInstance    :: TVar AriviP2PInstance,
    tvarPeerUUIDMap         :: TVar PeerUUIDMap,
    tqueueKadem             :: TQueue MessageInfo,
    tqueueRPC               :: TQueue MessageInfo,
    tqueuePubSub            :: TQueue MessageInfo,
    tvarConnectionInfoMap   :: TVar ConnectionInfoMap
}

type P2Papp = ReaderT P2PEnv IO

class (MonadIO m, MonadBaseControl IO m) => HasP2PEnv m where
    getP2PEnv                           :: m P2PEnv
    getAriviTVarP2PEnv                  :: m (TVar AriviP2PInstance)
    getpeerUUIDMapTVarP2PEnv            :: m (TVar PeerUUIDMap)
    getkademTQueueP2PEnv                :: m (TQueue MessageInfo)
    getrpcTQueueP2PEnv                  :: m (TQueue MessageInfo)
    getpubsubTQueueP2PEnv               :: m (TQueue MessageInfo)
    getConnectionInfoMapTVarP2PEnv      :: m (TVar ConnectionInfoMap)

instance HasP2PEnv P2Papp where
    getP2PEnv                       = ask
    getAriviTVarP2PEnv              = tvarAriviP2PInstance <$> getP2PEnv
    getpeerUUIDMapTVarP2PEnv        = tvarPeerUUIDMap <$> getP2PEnv
    getkademTQueueP2PEnv            = tqueueKadem <$> getP2PEnv
    getrpcTQueueP2PEnv              = tqueueRPC <$> getP2PEnv
    getpubsubTQueueP2PEnv           = tqueuePubSub <$> getP2PEnv
    getConnectionInfoMapTVarP2PEnv  = tvarConnectionInfoMap <$> getP2PEnv

runP2Papp :: P2PEnv -> P2Papp a -> IO a
runP2Papp = flip runReaderT

makeP2PEnvironment :: IO P2PEnv
makeP2PEnvironment = do

    pmap <- newTVarIO HM.empty
    kqueue <- newTQueueIO
    rqueue <- newTQueueIO
    pqueue <- newTQueueIO
    cmap <- newTVarIO HM.empty

    return P2PEnv {
        tvarPeerUUIDMap = pmap,
        tqueueKadem = kqueue,
        tqueueRPC = rqueue,
        tqueuePubSub = pqueue,
        tvarConnectionInfoMap = cmap
                }
