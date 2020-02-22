module SG.TextureCache where

import Control.Monad.IO.Class (MonadIO)
import SDL.Image (loadTexture)
import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (destroyTexture)
import SG.Cache

type TextureCache = Cache Texture

initTextureCache :: MonadIO m => Renderer -> m TextureCache
initTextureCache renderer = initCache (loadTexture renderer) destroyTexture

destroyTextureCache :: MonadIO m => TextureCache -> m ()
destroyTextureCache = destroyCache

loadTextureCached :: MonadIO m => TextureCache -> FilePath -> m Texture
loadTextureCached = loadCached
