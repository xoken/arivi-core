{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Arivi.Logging
  ( LogStatement (..)
  , withChanLogging
  , withLogging
  , withLoggingTH
  , withChanLoggingTH
  , LogLevel (..)
  , LogChan
  , HasLogging (..)
  , withIOLogging
  )
where

import           Control.Concurrent.STM
import           Control.Exception.Lifted    as CEL
import           Control.Exception           as CE
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Data.Monoid
import           Data.Text
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.CPUTime

type LogChan = TQueue (Loc, LogSource, LogLevel, Text)

data LogStatement =
    LogNetworkStatement Text

class ( MonadLogger m
      , MonadIO m
      , MonadBaseControl IO m
      , MonadThrow m
      , MonadCatch m
      ) =>
      HasLogging m where
    getLoggerChan :: m LogChan

toText :: LogStatement -> Text
toText (LogNetworkStatement l) = "LogNetworkStatement " <> l

withLoggingTH :: Q Exp
withLoggingTH = [|withLocLogging $(qLocation >>= liftLoc) |]

withChanLoggingTH :: Q Exp
withChanLoggingTH = [|withChanLocLogging $(qLocation >>= liftLoc) |]

withLocLogging ::
       (HasLogging m) => Loc -> LogStatement -> LogLevel -> m a -> m a
withLocLogging loc ls ll = logToF (monadLoggerLog loc (pack "") ll) (logOtherN ll) ls

withLogging :: (HasLogging m) => LogStatement -> LogLevel -> m a -> m a
withLogging = withLocLogging defaultLoc

withChanLocLogging ::
       (HasLogging m) => Loc -> LogStatement -> LogLevel -> m a -> m a
withChanLocLogging loc ls ll action = do
    logger <- getLoggerChan
    let lifts t = liftIO $ atomically $ writeTQueue logger (loc, pack "", ll, t)
    logToF lifts lifts ls action

withChanLogging :: (HasLogging m) => LogStatement -> LogLevel -> m a -> m a
withChanLogging = withChanLocLogging defaultLoc

logToF ::
       (MonadIO m, MonadBaseControl IO m, MonadThrow m)
    => (Text -> m ())
    -> (Text -> m ())
    -> LogStatement
    -> m a
    -> m a
logToF lf rf ls action = do
    (time, result) <- timeIt action
    case result of
        Left (e :: SomeException) -> do
            lf (pack $ displayException e)
            throwM e
        Right r -> do
            rf (toText ls <> " " <> pack (show time))
            return r

timeIt ::
       forall m a e. (MonadIO m, MonadBaseControl IO m, Exception e)
    => m a
    -> m (Double, Either e a)
timeIt ioa = do
    t1 <- liftIO getCPUTime
    a <- CEL.try ioa
    t2 <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (t2 - t1) * 1e-12
    return (t, a)

withIOLogging :: String -> IO a -> IO a
withIOLogging ls ioa = do
    result <- CE.try ioa
    case result of
        Left (e :: SomeException) -> do
            print (displayException e)
            throw e
        Right r -> do
            print ls
            return r
