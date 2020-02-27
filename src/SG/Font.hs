module SG.Font where

import Control.Exception.Lifted (bracket)
import Control.Lens ((^.), (^..), _2, folded, to, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Cache.LRU (LRU, insertInforming, lookup, newLRU, toList)
import Data.Foldable (for_)
import Data.IORef (IORef)
import Prelude hiding (lookup)
import SDL.Font (blended, free, load)
import SDL.Video (Renderer)
import SDL.Video.Renderer
  ( createTextureFromSurface
  , destroyTexture
  , freeSurface
  )
import SG.Cache (Cache, destroyCache, initCache, loadCached)
import SG.IORefLifted
import SG.TextureCache (SizedTexture(SizedTexture), stTexture, textureSize)
import SG.Types

type FontCache = Cache FontDescriptor SizedFont

loadFont :: MonadIO m => FontDescriptor -> m SizedFont
loadFont fd =
  liftIO
    (SizedFont <$> load (fd ^. fdFont) (fd ^. fdSize) <*> pure (fd ^. fdSize))

freeFont :: MonadIO m => SizedFont -> m ()
freeFont fd = liftIO (free (fd ^. sfFont))

initFontCache :: MonadIO m => m FontCache
initFontCache = initCache loadFont freeFont

destroyFontCache :: MonadIO m => FontCache -> m ()
destroyFontCache = destroyCache

loadFontCached :: MonadIO m => FontCache -> FontDescriptor -> m SizedFont
loadFontCached = loadCached

type TextCache = IORef (LRU TextDescriptor SizedTexture)

initTextCache :: MonadIO m => m TextCache
initTextCache = newIORef (newLRU (Just 32))

destroyTextCache :: MonadIO m => TextCache -> m ()
destroyTextCache textCacheRef = do
  textCache <- readIORef textCacheRef
  mapM_ destroyTexture (textCache ^.. to toList . folded . _2 . stTexture)

loadTextCached ::
     MonadIO m
  => Renderer
  -> FontCache
  -> TextCache
  -> TextDescriptor
  -> m SizedTexture
loadTextCached renderer fontCache textCacheRef rt = do
  textCache <- readIORef textCacheRef
  let (newCache, found) = lookup rt textCache
  case found of
    Just x -> do
      writeIORef textCacheRef newCache
      pure x
    Nothing -> do
      font <- loadFontCached fontCache (rt ^. rtFontDescriptor)
      texture <-
        liftIO $
        bracket
          (blended (font ^. sfFont) (rt ^. rtColor) (rt ^. rtText))
          freeSurface
          (createTextureFromSurface renderer)
      tSize <- textureSize texture
      let sizedTex = SizedTexture texture tSize
          (newNewCache, removedVal) = insertInforming rt sizedTex newCache
      for_ removedVal (destroyTexture . view (_2 . stTexture))
      writeIORef textCacheRef newNewCache
      pure sizedTex
