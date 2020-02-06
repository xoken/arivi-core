{-# LANGUAGE FlexibleContexts #-}

module Arivi.P2P.LevelDB
    ( getDBPath
    , getValue
    , putValue
    , deleteValue
    ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Default (def)
import Database.LevelDB (bloomFilter, createIfMissing, defaultOptions, delete, filterPolicy, get, open, put)

-- | This is path for the Database location
getDBPath :: String
getDBPath = "/tmp/lvlbloomtest"

-- | Returns Value from database corresponding to given key
getValue :: (MonadUnliftIO m) => ByteString -> m (Maybe ByteString)
getValue key =
    runResourceT $ do
        bloom <- bloomFilter 10
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
        get db def key

-- | Stores given (Key,Value) pair in database
putValue :: (MonadUnliftIO m) => ByteString -> ByteString -> m ()
putValue key value =
    runResourceT $ do
        bloom <- bloomFilter 10
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
        put db def key value
        return ()

-- | Deletes Value from database corresponding to given key
deleteValue :: (MonadUnliftIO m) => ByteString -> m ()
deleteValue key =
    runResourceT $ do
        bloom <- bloomFilter 10
        db <- open getDBPath defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
        delete db def key
        return ()
