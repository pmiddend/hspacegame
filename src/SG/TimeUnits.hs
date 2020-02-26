{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.TimeUnits where

import Data.Kind (Type)
import Data.Ratio (Ratio, (%), denominator, numerator)

data TimeUnit
  = Nanoseconds
  | Seconds

data Duration a (unit :: TimeUnit) =
  UnsafeMkDuration
    { durationValue :: a
    }

data SingTU :: TimeUnit -> Type where
  SNanoseconds :: SingTU 'Nanoseconds
  SSeconds :: SingTU 'Seconds

doubleSecond :: Double -> Duration Double 'Seconds
doubleSecond = UnsafeMkDuration

integerNanosecond :: Integer -> Duration Integer 'Nanoseconds
integerNanosecond = UnsafeMkDuration

durationRatio :: Integral a => SingTU s -> Ratio a
durationRatio sng =
  case sng of
    SNanoseconds -> 1 % 1000000000
    SSeconds -> 1

class SingTUI s where
  singTU :: SingTU s

instance SingTUI 'Nanoseconds where
  singTU = SNanoseconds

instance SingTUI 'Seconds where
  singTU = SSeconds

convertIntegral ::
     forall a b s1 s2. (Integral a, Integral b, SingTUI s1, SingTUI s2)
  => Duration a s1
  -> Duration b s2
convertIntegral (UnsafeMkDuration a) =
  let ratio :: Ratio a
      ratio =
        recip (durationRatio (singTU :: SingTU s1)) *
        durationRatio (singTU :: SingTU s2)
      rationalValue :: Ratio a
      rationalValue = ratio * fromIntegral a
   in UnsafeMkDuration
        (fromIntegral (numerator rationalValue `div` denominator rationalValue))
