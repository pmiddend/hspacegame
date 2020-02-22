{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module SG.Math where

import Control.Lens (Iso', Lens', (^.), iso, lens, makeLenses, set)
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((*^), (^/))
import SDL.Vect (Point(P))
import qualified SDL.Video.Renderer as SDL

data Rectangle a =
  Rectangle
    { _rectPos :: V2 a
    , _rectSize :: V2 a
    }
  deriving (Eq, Show, Functor)

makeLenses ''Rectangle

rectFromCenter :: Fractional a => V2 a -> V2 a -> Rectangle a
rectFromCenter center size = Rectangle (center - size ^/ 2) size

rectCenter :: Fractional a => Lens' (Rectangle a) (V2 a)
rectCenter =
  lens
    (\r -> (r ^. rectPos) + (r ^. rectSize) / 2)
    (\r c -> rectFromCenter c (r ^. rectSize))

rectLeft :: Lens' (Rectangle a) a
rectLeft = rectPos . _x

rectTop :: Lens' (Rectangle a) a
rectTop = rectPos . _y

rectW :: Lens' (Rectangle a) a
rectW = rectSize . _x

rectH :: Lens' (Rectangle a) a
rectH = rectSize . _y

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

rectIntersect :: (Num a, Ord a) => Rectangle a -> Rectangle a -> Bool
rectIntersect outer inner =
  let x1 = outer ^. rectLeft
      x2 = inner ^. rectLeft
      y1 = outer ^. rectTop
      y2 = inner ^. rectTop
      w1 = outer ^. rectW
      w2 = inner ^. rectW
      h1 = outer ^. rectH
      h2 = inner ^. rectH
   in not (x1 + w1 < x2 || x2 + w2 < x1 || y1 + h1 < y2 || y2 + h2 < y1)

rectEmbiggen :: Fractional a => a -> Rectangle a -> Rectangle a
rectEmbiggen f r = rectFromCenter (r ^. rectCenter) (f *^ (r ^. rectSize))
