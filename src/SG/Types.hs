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
import Control.Lens
  ( Getter
  , Lens'
  , (&)
  , (.~)
  , (^.)
  , lens
  , makeLenses
  , makePrisms
  , to
  )
import Data.Text (Text)
import Data.Time.Units (Millisecond)
import Data.Word (Word8)
import Linear.V2 (V2(V2))
import Linear.Vector ((^/))
import SDL.Vect (V4)
import SG.Math
import SG.Time
import SG.Util

type Color = V4 Word8

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
    (\b -> (b ^. bodyPosition) + (b ^. bodySize . floatingV2) ^/ 2.0)
    (\b newCenter ->
       b & bodyPosition .~ (newCenter - (b ^. bodySize . floatingV2) ^/ 2.0))

newtype Body =
  Body
    { _bodyData :: BodyData
    }
  deriving (Show)

instance Component Body where
  type Storage Body = Map Body

makeLenses ''Body

type Health = Int

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

data Lifetime =
  Lifetime
    { _lifetimeTotal :: Millisecond
    , _lifetimeEnd :: TimePoint
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

newtype EntityColor =
  EntityColor
    { _entityColor :: Color
    }
  deriving (Show)

makeLenses ''EntityColor

instance Component EntityColor where
  type Storage EntityColor = Map EntityColor

makeWorld
  "World"
  [ ''Body
  , ''Player
  , ''Target
  , ''Bullet
  , ''Image
  , ''Animation
  , ''Lifetime
  , ''EntityColor
  ]

type AllComponents = (Body, Player, Target, Bullet, Image, Animation)

type System' a = System World a

type Endo a = a -> a

type PlayerDirection = V2 Int

initialPlayerDirection :: PlayerDirection
initialPlayerDirection = V2 0 0

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

data GameState
  = GameStateRunning
  | GameStateOver
  deriving (Show, Eq)

makePrisms ''GameState
