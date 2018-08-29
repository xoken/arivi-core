--------------------------------------------------------------------------------
-- |
-- Module      : Arivi.P2P.LevelDB
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different functions that are used in management of
-- database
--
--------------------------------------------------------------------------------
module Arivi.P2P.LevelDB
    ( getValue
    , putValue
    , deleteValue
    ) where

import           Arivi.P2P.P2PEnv             (HasP2PEnv (..))
import           Control.Concurrent.STM.TVar  (readTVarIO)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString.Char8        (ByteString)
import           Data.Default                 (def)
import           Database.LevelDB             (DB, delete, get, put)

-- | Returns Value from database corresponding to given key
getValue :: (HasP2PEnv m, MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
getValue key =
    runResourceT $ do
        dbTVar <- liftIO getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        get db def key

-- | Stores given (Key,Value) pair in database
putValue :: (HasP2PEnv m, MonadIO m) => ByteString -> ByteString -> DB -> m ()
putValue key value =
    runResourceT $ do
        dbTVar <- liftIO getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        put db def key value

-- | Deletes Value from database corresponding to given key
deleteValue :: (HasP2PEnv m, MonadIO m) => ByteString -> DB -> m ()
deleteValue key =
    runResourceT $ do
        dbTVar <- getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        delete db def key
