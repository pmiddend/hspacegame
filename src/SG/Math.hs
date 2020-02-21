{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module SG.Math where

import Control.Lens (Iso', iso, makeLenses, set)
import Linear.V2 (V2(V2))
import SDL.Vect (Point(P))
import qualified SDL.Video.Renderer as SDL

data Rectangle a =
  Rectangle
    { _rectPos :: V2 a
    , _rectSize :: V2 a
    }
  deriving (Eq, Show, Functor)

makeLenses ''Rectangle

rootifyRect :: Num a => Rectangle a -> Rectangle a
rootifyRect = set rectPos (V2 0 0)

sdlRect :: Iso' (Rectangle a) (SDL.Rectangle a)
sdlRect = iso toSDL fromSDL
  where
    toSDL (Rectangle pos size) = SDL.Rectangle (P pos) size
    fromSDL (SDL.Rectangle (P pos) size) = Rectangle pos size

newtype Radians =
  Radians Double
  deriving (Show, Eq)

makeLenses ''Radians

newtype Degrees =
  Degrees
    { _getDegrees :: Double
    }
  deriving (Show, Eq)

makeLenses ''Degrees

degrees :: Iso' Radians Degrees
degrees = iso toDegrees fromDegrees
  where
    toDegrees (Radians x) = Degrees (x * 180.0 / pi)
    fromDegrees (Degrees x) = Radians (x / 180.0 * pi)
