{-# LANGUAGE TemplateHaskell #-}

module SG.TextureCache where

import Control.Lens (makeLenses, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Linear.V2 (V2(V2))
import SDL.Image (loadTexture)
import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer
  ( destroyTexture
  , queryTexture
  , textureHeight
  , textureWidth
  )
import SG.Cache

data SizedTexture =
  SizedTexture
    { _stTexture :: Texture
    , _stSize :: V2 Int
    }

makeLenses ''SizedTexture

type TextureCache = Cache FilePath SizedTexture

textureSize :: MonadIO m => Texture -> m (V2 Int)
textureSize t = do
  ti <- liftIO (queryTexture t)
  pure (V2 (fromIntegral (textureWidth ti)) (fromIntegral (textureHeight ti)))

loadSizedTexture :: MonadIO m => Renderer -> FilePath -> m SizedTexture
loadSizedTexture renderer fp = do
  t <- liftIO (loadTexture renderer fp)
  ts <- textureSize t
  pure (SizedTexture t ts)

destroySizedTexture :: MonadIO m => SizedTexture -> m ()
destroySizedTexture = destroyTexture . view stTexture

initTextureCache :: MonadIO m => Renderer -> m TextureCache
initTextureCache renderer =
  initCache (loadSizedTexture renderer) destroySizedTexture

destroyTextureCache :: MonadIO m => TextureCache -> m ()
destroyTextureCache = destroyCache

loadTextureCached :: MonadIO m => TextureCache -> FilePath -> m SizedTexture
loadTextureCached = loadCached
