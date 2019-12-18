{-# LANGUAGE FlexibleContexts #-}

<<<<<<< HEAD
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
=======
>>>>>>> breaking out arivi-core from arivi
module Arivi.P2P.LevelDB
    ( getDBPath
    , getValue
    , putValue
    , deleteValue
    ) where

<<<<<<< HEAD
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.ByteString              (ByteString)
import           Data.Default                 (def)
import           Database.LevelDB             (bloomFilter, createIfMissing,
                                               defaultOptions, delete,
                                               filterPolicy, get, open, put)
=======
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Default (def)
import Database.LevelDB (bloomFilter, createIfMissing, defaultOptions, delete, filterPolicy, get, open, put)
>>>>>>> breaking out arivi-core from arivi

-- | This is path for the Database location
getDBPath :: String
getDBPath = "/tmp/lvlbloomtest"

-- | Returns Value from database corresponding to given key
getValue :: (MonadUnliftIO m) => ByteString -> m (Maybe ByteString)
getValue key =
    runResourceT $ do
        bloom <- bloomFilter 10
<<<<<<< HEAD
        db <-
            open
                getDBPath
                defaultOptions
                {createIfMissing = True, filterPolicy = Just . Left $ bloom}
=======
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
>>>>>>> breaking out arivi-core from arivi
        get db def key

-- | Stores given (Key,Value) pair in database
putValue :: (MonadUnliftIO m) => ByteString -> ByteString -> m ()
putValue key value =
    runResourceT $ do
        bloom <- bloomFilter 10
<<<<<<< HEAD
        db <-
            open
                getDBPath
                defaultOptions
                {createIfMissing = True, filterPolicy = Just . Left $ bloom}
=======
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
>>>>>>> breaking out arivi-core from arivi
        put db def key value
        return ()

-- | Deletes Value from database corresponding to given key
deleteValue :: (MonadUnliftIO m) => ByteString -> m ()
deleteValue key =
    runResourceT $ do
        bloom <- bloomFilter 10
<<<<<<< HEAD
        db <-
            open
                getDBPath
                defaultOptions
                {createIfMissing = True, filterPolicy = Just . Left $ bloom}
=======
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
>>>>>>> breaking out arivi-core from arivi
        delete db def key
        return ()
