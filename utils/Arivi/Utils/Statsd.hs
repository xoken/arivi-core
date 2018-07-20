{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Arivi.Utils.Statsd
    ( StatsdClient
    , HasStatsdClient(..)
    , incrementCounter
    , decrementCounter
    , createStatsdClient
    , createStatsdClientWithHmac
    , StatsdM
    , counter
    , time
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Time.Units
import           Network.Socket              (PortNumber)
import qualified Network.Statsd              as S
import           Network.Statsd.UdpClient

-- | Structure to hold the stasd client
type StatsdClient = UdpClient

-- | Creates a statsd client which can be used to send unencypted messages
--   to a statsd server
createStatsdClient :: String -> PortNumber -> String -> IO StatsdClient
createStatsdClient hostname port prefix = do
    let str = "statsd://" ++ hostname ++ ":" ++ show port ++ "/" ++ prefix
    S.statsdClient str

-- | Creates a statsd client which can be used to send encrypted messages
--   to a statsd server using packet signing with shared secret HMAC SHA256
--   signatures
createStatsdClientWithHmac ::
       String -> Int -> String -> String -> IO StatsdClient
createStatsdClientWithHmac hostname port secret prefix = do
    let str =
            "statsd://:" ++
            secret ++ "@" ++ hostname ++ ":" ++ show port ++ "/" ++ prefix
    S.statsdClient str

-- | Abstract class which defines a function to get a statsd client
class (MonadIO m, MonadBaseControl IO m) =>
      HasStatsdClient m
    where
    getStatsdClient :: m StatsdClient

-- | Initiates a counter with a given value and sends the metric to statsd server
counter :: (HasStatsdClient m) => S.Stat -> Int -> m ()
counter label initV = do
    client <- getStatsdClient
    liftIO $ S.count client label initV

-- | Icrements and sends a counter given it's label
incrementCounter :: (HasStatsdClient m) => S.Stat -> m ()
incrementCounter label = do
    client <- getStatsdClient
    liftIO $ S.increment client label

-- Decrement and sends a counter given it's label
decrementCounter :: (HasStatsdClient m) => S.Stat -> m ()
decrementCounter label = do
    client <- getStatsdClient
    liftIO $ S.decrement client label

-- | Sends a timing metric to statsd server
time :: (HasStatsdClient m) => S.Stat -> Millisecond -> m ()
time label tms = do
    client <- getStatsdClient
    liftIO $ S.timing client label tms

type StatsdM = ReaderT StatsdClient IO

instance HasStatsdClient StatsdM where
    getStatsdClient = ask
