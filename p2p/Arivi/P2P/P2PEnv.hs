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

import           Control.Concurrent.STM                (TVar, newTVarIO)
import           Control.Concurrent.STM.TQueue
import           Control.Monad.IO.Class                (MonadIO)
import           Control.Monad.Reader                  (ReaderT, ask,
                                                        runReaderT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Data.HashMap.Strict                   as HM

data P2PEnv = P2PEnv
    { tvarAriviP2PInstance  :: TVar AriviP2PInstance
    , tvarNodeIdPeerMap     :: TVar NodeIdPeerMap
    , tqueueKadem           :: TQueue MessageInfo
    , tqueueRPC             :: TQueue MessageInfo
    , tqueuePubSub          :: TQueue MessageInfo
    , tvarResourceToPeerMap :: TVar ResourceToPeerMap
    }

type P2Papp = ReaderT P2PEnv IO

class (MonadIO m, MonadBaseControl IO m) =>
      HasP2PEnv m
    where
    getP2PEnv :: m P2PEnv
    getAriviTVarP2PEnv :: m (TVar AriviP2PInstance)
    getNodeIdPeerMapTVarP2PEnv :: m (TVar NodeIdPeerMap)
    getkademTQueueP2PEnv :: m (TQueue MessageInfo)
    getrpcTQueueP2PEnv :: m (TQueue MessageInfo)
    getpubsubTQueueP2PEnv :: m (TQueue MessageInfo)
    getResourceToPeerMapP2PEnv :: m (TVar ResourceToPeerMap)

instance HasP2PEnv P2Papp where
    getP2PEnv = ask
    getAriviTVarP2PEnv = tvarAriviP2PInstance <$> getP2PEnv
    getNodeIdPeerMapTVarP2PEnv = tvarNodeIdPeerMap <$> getP2PEnv
    getkademTQueueP2PEnv = tqueueKadem <$> getP2PEnv
    getrpcTQueueP2PEnv = tqueueRPC <$> getP2PEnv
    getpubsubTQueueP2PEnv = tqueuePubSub <$> getP2PEnv
    getResourceToPeerMapP2PEnv = tvarResourceToPeerMap <$> getP2PEnv

runP2Papp :: P2PEnv -> P2Papp a -> IO a
runP2Papp = flip runReaderT

makeP2PEnvironment :: IO P2PEnv
makeP2PEnvironment = do
    nmap <- newTVarIO HM.empty
    kqueue <- newTQueueIO
    rqueue <- newTQueueIO
    pqueue <- newTQueueIO
    r2pmap <- newTVarIO HM.empty
    return
        P2PEnv
            { tvarNodeIdPeerMap = nmap
            , tqueueKadem = kqueue
            , tqueueRPC = rqueue
            , tqueuePubSub = pqueue
            , tvarResourceToPeerMap = r2pmap
            }
