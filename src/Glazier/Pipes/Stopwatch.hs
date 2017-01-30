{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Glazier.Pipes.Stopwatch where

import Control.Lens
import Control.Monad.Trans
import qualified Pipes as P
import qualified System.Clock as C

-- | Record of epoch time and elapsed time
data Stopwatch = Stopwatch
    { stopwatchEpochTime :: {-# UNPACK #-}!C.TimeSpec -- | time since application epoch
    , stopwatchLapTime :: {-# UNPACK #-}!C.TimeSpec -- | time since last frame
    } deriving (Eq, Show)

makeFields ''Stopwatch

-- | Create a stopwatch pipe using first yielded time as the application epoch
-- and previous laptime
stopwatch :: MonadIO io => P.Pipe () Stopwatch io ()
stopwatch = do
    P.await
    t <- liftIO $ C.getTime C.ThreadCPUTime
    P.yield $ Stopwatch startTime startTime
    stopwatch' t t
  where
    startTime = C.TimeSpec 0 0

-- | Create a stopwatch Pipe using a given application epoch time and previous laptime
stopwatch' :: MonadIO io => C.TimeSpec -> C.TimeSpec -> P.Pipe () Stopwatch io ()
stopwatch' epoch prev = go prev
  where
    go prev' = do
        () <- P.await
        t <- liftIO $ C.getTime C.ThreadCPUTime
        P.yield $ Stopwatch (C.diffTimeSpec t epoch) (C.diffTimeSpec t prev')
        go t
