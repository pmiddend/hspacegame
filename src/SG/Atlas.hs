{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Atlas where

import Control.Lens
  ( (&)
  , (.~)
  , (<.)
  , (^?!)
  , (^@..)
  , _2
  , at
  , makeLenses
  , over
  , set
  , to
  , view
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Integer, key, members)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map, fromList)
import Data.Text (Text)
import Linear.V2 (V2(V2))
import SDL.Video (Texture)
import SG.Math (Rectangle(Rectangle))
import SG.TextureCache (TextureCache, loadTextureCached)
import System.FilePath (FilePath)
import System.FilePath.Lens (extension)

type AtlasFrame = Rectangle Int

data Atlas =
  Atlas
    { _atlasFrames :: Map Text AtlasFrame
    , _atlasTexture :: Texture
    }

makeLenses ''Atlas

loadJsonRect :: Value -> Rectangle Int
loadJsonRect v =
  let loadInt :: Text -> Int
      loadInt name = v ^?! key name . _Integer . to fromIntegral
   in Rectangle
        (V2 (loadInt "x") (loadInt "y"))
        (V2 (loadInt "w") (loadInt "h"))

loadAtlas :: MonadIO m => FilePath -> TextureCache -> m Atlas
loadAtlas fp cache = do
  let jsonFile = fp & extension .~ ".json"
  frames <- loadAtlasFrames jsonFile
  liftIO (putStrLn ("loading atlas json " <> jsonFile))
  texture <- loadTextureCached cache fp
  pure (Atlas frames texture)

loadAtlasFrames :: MonadIO m => FilePath -> m (Map Text AtlasFrame)
loadAtlasFrames fp = do
  contents <- liftIO (readFile fp)
  pure
    (fromList
       (contents ^@.. key "frames" . members <. key "frame" . to loadJsonRect))

type AtlasCache = IORef (TextureCache, Map FilePath Atlas)

initAtlasCache :: TextureCache -> IO AtlasCache
initAtlasCache cache = newIORef (cache, mempty)

destroyAtlasCache :: AtlasCache -> IO ()
destroyAtlasCache _ = pure ()

loadAtlasCached :: MonadIO m => AtlasCache -> FilePath -> m Atlas
loadAtlasCached cache fp = do
  (textureCache, existingAtlas) <-
    over _2 (view (at fp)) <$> liftIO (readIORef cache)
  case existingAtlas of
    Just atlas -> pure atlas
    Nothing -> do
      liftIO (putStrLn ("loading atlas from file " <> fp))
      loadedAtlas <- loadAtlas fp textureCache
      liftIO (modifyIORef cache (set (_2 . at fp) (Just loadedAtlas)))
      pure loadedAtlas
