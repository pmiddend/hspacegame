{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SG.Starfield where

import Control.Lens ((%~), (&), (^.), (^?!), ix, makeLenses, to, traversed)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random (runRand)
import Control.Monad.Random.Class (MonadRandom, getRandomR)
import Data.Foldable (forM_)
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((*^))
import Numeric.Lens (negated)
import SDL.Video (Renderer)
import SDL.Video.Renderer (copyEx)
import SG.Atlas
import SG.Constants
import SG.Math
import SG.Types (Endo)
import System.Random (StdGen)

data StarLayer =
  StarLayer
    { _starLayerPositions :: [V2 Double]
    , _starLayerSize :: V2 Int
    , _starLayerVelocity :: V2 Double
    }

data Starfield =
  Starfield
    { _starLayers :: [StarLayer]
    , _starRng :: StdGen
    }

makeLenses ''StarLayer

makeLenses ''Starfield

generateStarLayer :: MonadRandom m => Int -> V2 Int -> V2 Double -> m StarLayer
generateStarLayer number size velocity = do
  positions <- replicateM number randomPosition
  pure (StarLayer positions size velocity)
  where
    randomX :: MonadRandom m => m Double
    randomX = fromIntegral <$> getRandomR (0, gameSize ^. _x)
    randomY :: MonadRandom m => m Double
    randomY = fromIntegral <$> getRandomR (0, gameSize ^. _y)
    randomPosition :: MonadRandom m => m (V2 Double)
    randomPosition = V2 <$> randomX <*> randomY

starCountPerLayer :: Int -> Int
starCountPerLayer i = 65 * (i + 1) - 60

starSizePerLayer :: Int -> V2 Int
starSizePerLayer i = V2 size size
  where
    size = 26 - 7 * (i + 1)

starVelocityPerLayer :: Int -> V2 Double
starVelocityPerLayer i = V2 0 (266 - 75 * fromIntegral (i + 1))

initStarfield :: StdGen -> Starfield
initStarfield gen =
  let generate =
        mapM
          (\i ->
             generateStarLayer
               (starCountPerLayer i)
               (starSizePerLayer i)
               (starVelocityPerLayer i))
          [0 .. 2]
      (layers, finalGen) = runRand generate gen
   in Starfield layers finalGen

stepStarfield :: Double -> Endo Starfield
stepStarfield timeDelta field = field & starLayers . traversed %~ editLayer
  where
    editLayer layer =
      layer & starLayerPositions . traversed %~ editStar layer timeDelta
    editStar :: StarLayer -> Double -> Endo (V2 Double)
    editStar layer delta pos
      | outOfBounds (layer ^. starLayerSize) pos =
        V2 (pos ^. _x) (layer ^. starLayerSize . _y . negated . to fromIntegral)
      | otherwise = pos + delta *^ (layer ^. starLayerVelocity)
    outOfBounds :: V2 Int -> V2 Double -> Bool
    outOfBounds size pos =
      not (rectIntersect gameRect (Rectangle (round <$> pos) size))

drawStarfield :: MonadIO m => Renderer -> AtlasCache -> Starfield -> m ()
drawStarfield renderer cache field = do
  foundAtlas <- loadAtlasCached cache mainAtlasPath
  forM_ (field ^. starLayers) $ \layer ->
    forM_ (layer ^. starLayerPositions) $ \pos ->
      copyEx
        renderer
        (foundAtlas ^. atlasTexture)
        (Just
           (foundAtlas ^?! atlasFrames . ix "star.png" . to (fromIntegral <$>) .
            sdlRect))
        (Just
           (Rectangle
              (round <$> pos)
              (fromIntegral <$> (layer ^. starLayerSize)) ^.
            sdlRect))
        0
        Nothing
        (V2 False False)
