{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SG.Time where

import Control.Lens (makeLenses)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ratio ((%))
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)

newtype Duration =
  Duration
    { _duration :: Integer
    }
  deriving (Show, Eq, Ord)

makeLenses ''Duration

nanoseconds :: Integral a => a -> Duration
nanoseconds = Duration . fromIntegral

microseconds :: Integral a => a -> Duration
microseconds = Duration . fromIntegral . (* 1000)

milliseconds :: Integral a => a -> Duration
milliseconds = Duration . fromIntegral . (* 1000000)

seconds :: Integral a => a -> Duration
seconds = Duration . fromIntegral . (* 1000000000)

newtype TimePoint =
  TimePoint
    { _timePoint :: Duration
    }
  deriving (Eq, Ord, Show)

makeLenses ''TimePoint

getNow :: MonadIO m => m TimePoint
getNow = liftIO (TimePoint . nanoseconds . toNanoSecs <$> getTime Monotonic)

(@*) :: Integral a => a -> Duration -> Duration
a @* (Duration x) = Duration (fromIntegral a * x)

(@+) :: Integral a => a -> Duration -> Duration
a @+ (Duration x) = Duration (fromIntegral a + x)

timeDiff :: TimePoint -> TimePoint -> Duration
timeDiff (TimePoint (Duration x)) (TimePoint (Duration y)) = nanoseconds (x - y)

durationDiv :: Duration -> Duration -> Double
durationDiv (Duration a) (Duration b) = fromRational (a % b)

durationDivInt :: Duration -> Duration -> Integer
durationDivInt (Duration a) (Duration b) = a `div` b

($+) :: Duration -> TimePoint -> TimePoint
($+) (Duration d) (TimePoint (Duration e)) = TimePoint (nanoseconds (d + e))
