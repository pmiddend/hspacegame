{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SG.Types where

import Apecs
import Control.Lens (Getter, Lens', (&), (.~), (^.), lens, makeLenses, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Units (Millisecond, TimeUnit, fromMicroseconds, toMicroseconds)
import Data.Word (Word8)
import Linear.V2 (V2(V2))
import Linear.Vector ((^/))
import SDL.Vect (V4)
import SG.Math
import System.Clock
  ( Clock(Monotonic)
  , TimeSpec
  , diffTimeSpec
  , fromNanoSecs
  , getTime
  , toNanoSecs
  )

data BodyData =
  BodyData
    { _bodyPosition :: V2 Double
    , _bodySize :: V2 Int
    , _bodyAngle :: Radians
    , _bodyVelocity :: V2 Double
    , _bodyAngularVelocity :: Radians
    }
  deriving (Show)

makeLenses ''BodyData

bodyCenter :: Lens' BodyData (V2 Double)
bodyCenter =
  lens
    (\b -> (b ^. bodyPosition) + (fromIntegral <$> (b ^. bodySize)) ^/ 2.0)
    (\b newCenter ->
       b & bodyPosition .~
       (newCenter - (fromIntegral <$> (b ^. bodySize)) ^/ 2.0))

newtype Body =
  Body
    { _bodyData :: BodyData
    }
  deriving (Show)

instance Component Body where
  type Storage Body = Map Body

makeLenses ''Body

type Health = Int

type TimePoint = TimeSpec

bodyRectangle :: Lens' Body (Rectangle Double)
bodyRectangle =
  lens
    (\b ->
       Rectangle
         (b ^. bodyData . bodyPosition)
         (b ^. bodyData . bodySize . to (fromIntegral <$>)))
    (\b r ->
       b & (bodyData . bodyPosition .~ (r ^. rectPos)) & bodyData . bodySize .~
       (round <$> (r ^. rectSize)))

data ImageIdentifier =
  ImageIdentifier
    { _iiAtlasPath :: FilePath
    , _iiAtlasName :: Text
    }
  deriving (Show)

makeLenses ''ImageIdentifier

newtype Image =
  Image ImageIdentifier
  deriving (Show)

instance Component Image where
  type Storage Image = Map Image

data AnimationIdentifier =
  AnimationIdentifier
    { _aiAtlasPath :: FilePath
    , _aiFrameCount :: Int
    , _aiFrameSize :: V2 Int
    , _aiFrameDuration :: Millisecond
    }
  deriving (Show)

makeLenses ''AnimationIdentifier

aiTotalDuration :: Getter AnimationIdentifier Millisecond
aiTotalDuration =
  to (\ai -> fromIntegral (ai ^. aiFrameCount) * (ai ^. aiFrameDuration))

data Animation =
  Animation
    { _animationIdentifier :: AnimationIdentifier
    , _animationStart :: TimePoint
    }
  deriving (Show)

makeLenses ''Animation

instance Component Animation where
  type Storage Animation = Map Animation

data Target =
  Target
    { _targetRadius :: Double
    , _targetHealth :: Health
    }
  deriving (Show)

makeLenses ''Target

instance Component Target where
  type Storage Target = Map Target

data Bullet =
  Bullet
    { _bulletRadius :: Double
    , _bulletHealth :: Health
    }
  deriving (Show)

makeLenses ''Bullet

instance Component Bullet where
  type Storage Bullet = Map Bullet

newtype Lifetime =
  Lifetime
    { _lifetimeEnd :: TimePoint
    }
  deriving (Show)

makeLenses ''Lifetime

instance Component Lifetime where
  type Storage Lifetime = Map Lifetime

data Player =
  Player
  deriving (Show)

instance Component Player where
  type Storage Player = Unique Player

makeWorld
  "World"
  [''Body, ''Player, ''Target, ''Bullet, ''Image, ''Animation, ''Lifetime]

type AllComponents = (Body, Player, Target, Bullet, Image, Animation)

type System' a = System World a

type Endo a = a -> a

type PlayerDirection = V2 Int

initialPlayerDirection :: PlayerDirection
initialPlayerDirection = V2 0 0

getNow :: MonadIO m => m TimePoint
getNow = liftIO (getTime Monotonic)

instance TimeUnit a => Act a TimePoint where
  duration ~^ tp = tp + fromNanoSecs (toMicroseconds duration * 1000)

-- TODO: use "acts" to define algebras on time points and durations
timeDiff :: TimeUnit a => TimePoint -> TimePoint -> a
timeDiff a b = fromMicroseconds (toNanoSecs (a `diffTimeSpec` b) `div` 1000)

instance TimeUnit a => Act Integer a where
  x ~^ duration = fromMicroseconds (x * toMicroseconds duration)

data SpawnType =
  SpawnTypeAsteroidMedium

data Spawn =
  Spawn
    { _spawnTimeDiff :: Millisecond
    , _spawnType :: SpawnType
    , _spawnPosition :: V2 Double
    }

makeLenses ''Spawn

type Level = [Spawn]

newtype Score =
  Score
    { _score :: Int
    }
  deriving (Num, Enum, Integral, Real, Ord, Eq)

makeLenses ''Score

newtype Energy =
  Energy
    { _energy :: Double
    }
  deriving (Num, Enum, Ord, Eq, Real, RealFrac, Fractional, Floating, RealFloat)

type Color = V4 Word8
