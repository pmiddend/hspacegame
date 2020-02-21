module SG.TextureCache where

import Control.Lens (_2, at, folded, over, set, toListOf, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import SDL.Image (loadTexture)
import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (destroyTexture)

type TextureCache = IORef (Renderer, Map FilePath Texture)

initTextureCache :: Renderer -> IO TextureCache
initTextureCache renderer = newIORef (renderer, mempty)

destroyTextureCache :: TextureCache -> IO ()
destroyTextureCache cache = do
  textures <- toListOf (_2 . folded) <$> readIORef cache
  traverse_ destroyTexture textures

loadTextureCached :: MonadIO m => TextureCache -> FilePath -> m Texture
loadTextureCached cache fp = do
  (renderer, existingTexture) <-
    over _2 (view (at fp)) <$> liftIO (readIORef cache)
  case existingTexture of
    Nothing -> do
      liftIO (putStrLn ("loading texture from file " <> fp))
      texture <- loadTexture renderer fp
      liftIO (modifyIORef cache (set (_2 . at fp) (Just texture)))
      pure texture
    Just texture -> pure texture
