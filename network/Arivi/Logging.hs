{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Arivi.Logging
  ( LogStatement (..)
  , withLoggingTH
  , withChanLoggingTH
  , LogLevel (..)
  , LogChan
  , HasLogging (..)
  )
where

import           Control.Concurrent.STM
import           Control.Exception          as CE
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Monoid
import           Data.Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.CPUTime

type LogChan = TQueue (Loc, LogSource, LogLevel, Text)

data LogStatement = LogNetworkStatement Text

class (MonadLogger m, MonadIO m, MonadThrow m, MonadCatch m) => HasLogging m where
  getLoggerChan :: m LogChan

toText :: LogStatement -> Text
toText (LogNetworkStatement l) = "LogNetworkStatement " <> l

withLoggingTH :: Q Exp
withLoggingTH = [|withLocLogging $(qLocation >>= liftLoc) |]

withChanLoggingTH :: Q Exp
withChanLoggingTH = [|withChanLocLogging $(qLocation >>= liftLoc) |]

withLocLogging :: (HasLogging m) => Loc -> LogStatement -> LogLevel -> IO a -> m a
withLocLogging loc ls ll = logToF (monadLoggerLog loc (pack "") ll) (logOtherN ll) ls ll

withLogging :: (HasLogging m) => LogStatement -> LogLevel -> IO a -> m a
withLogging = withLocLogging defaultLoc

withChanLocLogging :: (HasLogging m) => Loc -> LogStatement -> LogLevel -> IO a -> m a
withChanLocLogging loc ls ll action = do
  logger <- getLoggerChan
  let lifts t = liftIO $ atomically $ writeTQueue logger (loc, pack "", ll, t)
  logToF lifts lifts ls ll action

withChanLogging :: (HasLogging m) => LogStatement -> LogLevel -> IO a -> m a
withChanLogging = withChanLocLogging defaultLoc

logToF :: (MonadIO m, MonadThrow m) => (Text -> m ()) -> (Text -> m ()) -> LogStatement -> LogLevel -> IO a -> m a
logToF lf rf ls ll action = do
  (time, result) <- liftIO $ timeIt action
  case result of
    Left (e :: SomeException) -> do
      lf "An exception has occurred"
      throwM e
    Right r -> do
      rf (toText ls <> " " <> pack (show time))
      return r

timeIt :: (Exception e) => IO a -> IO (Double, Either e a)
timeIt ioa = do
  t1 <- liftIO getCPUTime
  a <- CE.try ioa
  t2 <- liftIO getCPUTime
  let t :: Double
      t = fromIntegral (t2 - t1) * 1e-12
  return (t, a)
