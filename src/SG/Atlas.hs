{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Atlas where

import Control.Lens ((&), (.~), (<.), (^?!), (^@..), makeLenses, to)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Integer, key, members)
import Data.Map.Strict (Map, fromList)
import Data.Text (Text)
import Linear.V2 (V2(V2))
import SG.Cache
import SG.Math (Rectangle(Rectangle))
import SG.TextureCache (SizedTexture, TextureCache, loadTextureCached)
import System.FilePath (FilePath)
import System.FilePath.Lens (extension)

type AtlasFrame = Rectangle Int

data Atlas =
  Atlas
    { _atlasFrames :: Map Text AtlasFrame
    , _atlasTexture :: SizedTexture
    }

makeLenses ''Atlas

loadJsonRect :: Value -> Rectangle Int
loadJsonRect v =
  let loadInt :: Text -> Int
      loadInt name = v ^?! key name . _Integer . to fromIntegral
   in Rectangle
        (V2 (loadInt "x") (loadInt "y"))
        (V2 (loadInt "w") (loadInt "h"))

loadAtlas :: TextureCache -> FilePath -> IO Atlas
loadAtlas cache fp = do
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

type AtlasCache = Cache Atlas

initAtlasCache :: MonadIO m => TextureCache -> m AtlasCache
initAtlasCache textureCache =
  initCache (loadAtlas textureCache) (const (pure ()))

destroyAtlasCache :: MonadIO m => AtlasCache -> m ()
destroyAtlasCache = destroyCache

loadAtlasCached :: MonadIO m => AtlasCache -> FilePath -> m Atlas
loadAtlasCached = loadCached
