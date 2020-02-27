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
import SDL.Font (Font, PointSize)
import SDL.Vect (V4)
import SG.Math
import SG.Time
import SG.Util

type Color = V4 Word8

data FontDescriptor =
  FontDescriptor
    { _fdFont :: FilePath
    , _fdSize :: PointSize
    }
  deriving (Ord, Eq, Show)

makeLenses ''FontDescriptor

data SizedFont =
  SizedFont
    { _sfFont :: Font
    , _sfSize :: PointSize
    }

makeLenses ''SizedFont

data TextDescriptor =
  TextDescriptor
    { _rtFontDescriptor :: FontDescriptor
    , _rtColor :: Color
    , _rtText :: Text
    }
  deriving (Eq, Show, Ord)

makeLenses ''TextDescriptor

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

newtype BodyComponent =
  BodyComponent
    { _bodyData :: BodyData
    }
  deriving (Show)

instance Component BodyComponent where
  type Storage BodyComponent = Map BodyComponent

makeLenses ''BodyComponent

type Health = Int

bodyRectangle :: Lens' BodyComponent (Rectangle Double)
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

data Image
  = StillImage ImageIdentifier
  | DynamicImage Animation
  deriving (Show)

newtype ImageComponent =
  ImageComponent Image
  deriving (Show)

instance Component ImageComponent where
  type Storage ImageComponent = Map ImageComponent

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

data Ramp
  = LinearFromTo Double Double
  | UpThenDown Double Double Double Double
  deriving (Show)

data Lifetime =
  Lifetime
    { _lifetimeTotal :: Millisecond
    , _lifetimeEnd :: TimePoint
    , _lifetimeRamp :: Maybe Ramp
    }
  deriving (Show)

makeLenses ''Lifetime

instance Component Lifetime where
  type Storage Lifetime = Map Lifetime

newtype ColorComponent =
  ColorComponent
    { _entityColor :: Color
    }
  deriving (Show)

makeLenses ''ColorComponent

instance Component ColorComponent where
  type Storage ColorComponent = Map ColorComponent

data PositionedText =
  PositionedText
    { _textDescriptor :: TextDescriptor
    , _textPosition :: V2 Int
    }
  deriving (Show)

makeWorld
  "World"
  [ ''BodyComponent
  , ''Target
  , ''Bullet
  , ''ImageComponent
  , ''Lifetime
  , ''ColorComponent
  ]

type AllComponents = (BodyComponent, Target, Bullet, ImageComponent)

type System' a = System World a

type Endo a = a -> a

type PlayerDirection = V2 Int

initialPlayerDirection :: PlayerDirection
initialPlayerDirection = V2 0 0

data SpawnType
  = SpawnTypeAsteroidMedium (V2 Double)
  | SpawnTypeHint TextDescriptor

data Spawn =
  Spawn
    { _spawnTimeDiff :: Millisecond
    , _spawnType :: SpawnType
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

data Hint =
  Hint
    { _hintText :: TextDescriptor
    , _hintDuration :: Millisecond
    , _hintEnd :: TimePoint
    }

makeLenses ''Hint
