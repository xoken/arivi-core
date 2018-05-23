{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Arivi.Logging
  ( LogStatement (..)
  , withLogging
  , LogLevel (..)
  )
where

import Arivi.Env
import Control.Exception as CE
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text
import Control.Monad.Logger
import System.CPUTime
import Data.Monoid

data LogStatement = LogNetworkStatement Text

toText :: LogStatement -> Text
toText (LogNetworkStatement l) = l


withLogging :: (HasLogging m) => LogStatement -> LogLevel -> IO a -> m a
withLogging ls ll action = do
  (time, result) <- liftIO $ timeIt action
  case result of
    Left (e :: SomeException) -> do
      logErrorN "An exception has occurred"
      throwM e
    Right r -> do
      logOtherN ll (toText ls <> " " <> (pack $ show time))
      return r

timeIt :: (Exception e) => IO a -> IO (Double, Either e a)
timeIt ioa = do
  t1 <- liftIO getCPUTime
  a <- CE.try $ ioa
  t2 <- liftIO getCPUTime
  let t :: Double
      t = fromIntegral (t2 - t1) * 1e-12
  return (t, a)
