{-# LANGUAGE FlexibleContexts #-}

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
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.ByteString.Char8        (ByteString)
import           Data.Default                 (def)
import           Database.LevelDB             (delete, get, put)

--
import           Control.Monad.IO.Unlift      (MonadUnliftIO)

-- | Returns Value from database corresponding to given key
getValue ::
       (MonadUnliftIO m, HasP2PEnv (ResourceT m))
    => ByteString
    -> m (Maybe ByteString)
getValue key =
    runResourceT $ do
        dbTVar <- getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        get db def key

-- | Stores given (Key,Value) pair in database
putValue ::
       (MonadUnliftIO m, HasP2PEnv (ResourceT m))
    => ByteString
    -> ByteString
    -> m ()
putValue key value =
    runResourceT $ do
        dbTVar <- getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        put db def key value
        return ()

-- | Deletes Value from database corresponding to given key
deleteValue :: (MonadUnliftIO m, HasP2PEnv (ResourceT m)) => ByteString -> m ()
deleteValue key =
    runResourceT $ do
        dbTVar <- getDBTVar
        db <- liftIO $ readTVarIO dbTVar
        delete db def key
        return ()
