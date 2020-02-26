{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module SG.TimeUnits where

import Data.Ratio ((%))

data TSeconds

data TNanoseconds

data TimeUnit
  = Nanoseconds
  | Seconds

data WitnessUnit a where
  WitnessSeconds :: WitnessUnit 'Seconds
  WitnessNanoseconds :: WitnessUnit 'Nanoseconds

data Duration a (unit :: TimeUnit) =
  Duration
    { durationValue :: a
    , durationUnit :: WitnessUnit unit
    }

doubleSecond :: Double -> Duration Double 'Seconds
doubleSecond x = Duration x WitnessSeconds

integerNanosecond :: Integer -> Duration Integer 'Nanoseconds
integerNanosecond y = Duration y WitnessNanoseconds

durationDiff :: Num a => Duration a k -> Duration a k -> Duration a k
durationDiff (Duration x w) (Duration y _) = Duration (x - y) w

class RatioGiver a where
  giveRatio :: a -> Rational

instance RatioGiver TSeconds where
  giveRatio _ = 1

instance RatioGiver TNanoseconds where
  giveRatio _ = 1000000000

-- type family TimeRatio c
-- type instance TimeRatio 'Seconds = '1
-- factor :: WitnessUnit a -> Rational
-- factor WitnessSeconds = 1
-- factor WitnessNanoseconds = 1 % 1000000000
-- factor :: a -> Rational
-- factor '
-- Example: Nanoseconds Int to Seconds Double
-- do "* factor Nanoseconds / factor Seconds"
convert :: Duration a u1 -> Duration b u2
convert (Duration x w) = factor w * factor w2
