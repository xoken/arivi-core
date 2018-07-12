-- |
-- Module      : Arivi.Kademlia.RefreshKbucket
-- Copyright   : (c) Xoken Labs
-- License     : -
--
-- Maintainer  : Ankit Singh {ankitsiam@gmail.com}
-- Stability   : experimental
-- Portability : portable
--
-- This module provides functionality to refresh k-bucket after a fixed
-- time, this is necessary because in P2P Networks nodes go offline
-- all the time that's why it is essential to make sure that active
-- peers are prioritised, this module do that by issuing the PING request
-- to the k-bucket entries and shuffle the list based on the response
--
module Arivi.P2P.Kademlia.RefreshKbucket
    (
    ) where
-- import           Arivi.P2P.Kademlia.Kbucket
-- import           Arivi.P2P.Kademlia.Types
-- import           Arivi.P2P.P2PEnv
-- import           Control.Concurrent         (threadDelay)
-- import           Control.Monad              (forever)
-- import           Control.Monad.IO.Class     (liftIO)
-- refreshKbucket :: (HasKbucket m) => Int -> m ()
-- refreshKbucket timeDelay = forever $ do
--                                 threadDelay timeDelay
--                                 liftIO $ atomically $ do
