module SG.Time where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Units (TimeUnit, fromMicroseconds, toMicroseconds)
import System.Clock
  ( Clock(Monotonic)
  , TimeSpec
  , diffTimeSpec
  , fromNanoSecs
  , getTime
  , toNanoSecs
  )

type TimePoint = TimeSpec

getNow :: MonadIO m => m TimePoint
getNow = liftIO (getTime Monotonic)

addDuration :: TimeUnit a => a -> TimePoint -> TimePoint
addDuration duration tp = tp + fromNanoSecs (toMicroseconds duration * 1000)

timeDiff :: TimeUnit a => TimePoint -> TimePoint -> a
timeDiff a b = fromMicroseconds (toNanoSecs (a `diffTimeSpec` b) `div` 1000)

multiplyDuration :: TimeUnit a => Integer -> a -> a
multiplyDuration x duration = fromMicroseconds (x * toMicroseconds duration)
