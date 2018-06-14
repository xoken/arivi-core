--{-# LANGUAGE DeriveGeneric         #-}

-- |
-- Module      :  Arivi.P2P.ServiceRegistry
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- ServiceRegistry is part of Arivi P2P layer, It keeps track of ServiceContext
-- and ConnectionCommand
module Arivi.P2P.ServiceRegistry
(
    makeP2Pinstance
) where

import              Arivi.P2P.P2PEnv
import              Arivi.P2P.Types

import              Control.Concurrent
import              Control.Concurrent.STM




makeP2Pinstance :: NodeId -> IP -> Port -> Port -> IO ()
makeP2Pinstance nodeid ip tcpport udpport = do
    ariviP2PInstanceTvar <-  atomically( newTVar  ( AriviP2PInstance nodeid ip tcpport udpport ))
    p2p' <- makeP2PEnvironment
    let p2pEnv = p2p' { tvarAriviP2PInstance = ariviP2PInstanceTvar}
    liftIO $ forkIO $ runP2Papp p2pEnv (
--add funcs to run at start
            )
    return()