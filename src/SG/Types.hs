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
import Control.Lens (makeLenses)
import Data.Text (Text)
import Linear.V2 (V2)
import SG.Math (Radians)

data Body =
  Body
    { _bodyPosition :: V2 Double
    , _bodySize :: V2 Int
    , _bodyAngle :: Radians
    , _bodyVelocity :: V2 Double
    , _bodyAngularVelocity :: Radians
    }
  deriving (Show)

instance Component Body where
  type Storage Body = Map Body

makeLenses ''Body

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

newtype Score =
  Score Int
  deriving (Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

instance Component Score where
  type Storage Score = Global Score

makeWorld "World" [''Body, ''Player, ''Target, ''Bullet, ''Score, ''Image]

type System' a = System World a

type Endo a = a -> a

-- data PlayerKeys =
--   PlayerKeys
--     { _pkLeft :: Bool
--     , _pkRight :: Bool
--     , _pkUp :: Bool
--     , _pkDown :: Bool
--     } deriving(Show)

-- makeLenses ''PlayerKeys

-- initialPlayerKeys :: PlayerKeys
-- initialPlayerKeys = PlayerKeys False False False False
