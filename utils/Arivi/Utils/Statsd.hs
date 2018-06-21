{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arivi.Utils.Statsd
    ( StatsdClient(..)
    , HasStatsdClient(..)
    , incrementCounter
    , decrementCounter
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Network.Statsd              as S
import           Network.Statsd.UdpClient

-- | Structure to hold the stasd client
type StatsdClient = UdpClient

-- | Abstract class which defines a function to get a statsd client
class (MonadIO m, MonadBaseControl IO m) =>
      HasStatsdClient m
    where
    getStatsdClient :: m StatsdClient

-- | icrements and sends a counter given it's label
incrementCounter :: (HasStatsdClient m) => S.Stat -> m ()
incrementCounter label = do
    client <- getStatsdClient
    liftIO $ S.increment client label

-- decrement and sends a counter given it's label
decrementCounter :: (HasStatsdClient m) => S.Stat -> m ()
decrementCounter label = do
    client <- getStatsdClient
    liftIO $ S.decrement client label

type AppM = ReaderT StatsdClient IO

instance HasStatsdClient AppM where
    getStatsdClient = ask
