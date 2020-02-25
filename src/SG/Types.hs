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
import Control.Lens (Lens', (&), (.~), (^.), lens, makeLenses, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time.Units (Millisecond, TimeUnit, fromMicroseconds)
import Linear.V2 (V2(V2))
import SG.Math
import System.Clock
  ( Clock(Monotonic)
  , TimeSpec
  , diffTimeSpec
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

newtype Body =
  Body
    { _bodyData :: BodyData
    }
  deriving (Show)

instance Component Body where
  type Storage Body = Map Body

makeLenses ''Body

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

data Target =
  Target
  deriving (Show)

instance Component Target where
  type Storage Target = Map Target

data Bullet =
  Bullet
  deriving (Show)

instance Component Bullet where
  type Storage Bullet = Map Bullet

data Player =
  Player
  deriving (Show)

instance Component Player where
  type Storage Player = Unique Player

makeWorld "World" [''Body, ''Player, ''Target, ''Bullet, ''Image]

type AllComponents = (Body, Player, Target, Bullet, Image)

type System' a = System World a

type Endo a = a -> a

type PlayerDirection = V2 Int

initialPlayerDirection :: PlayerDirection
initialPlayerDirection = V2 0 0

type TimePoint = TimeSpec

getNow :: MonadIO m => m TimePoint
getNow = liftIO (getTime Monotonic)

-- TODO: use "acts" to define algebras on time points and durations
timeDiff :: TimeUnit a => TimePoint -> TimePoint -> a
timeDiff a b = fromMicroseconds (toNanoSecs (a `diffTimeSpec` b) `div` 1000)

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
