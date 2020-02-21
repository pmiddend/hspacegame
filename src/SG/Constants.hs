{-# LANGUAGE OverloadedStrings #-}

module SG.Constants where

import Linear.V2 (V2(V2))
import SG.Math (Rectangle(Rectangle))
import SG.Types (ImageIdentifier(..))
import System.FilePath ((</>))

gameSize :: V2 Int
gameSize = V2 1024 768

gameRect :: Rectangle Int
gameRect = Rectangle (V2 0 0) gameSize

playerSize :: V2 Int
playerSize = V2 50 32

basePath :: FilePath
basePath = "data"

backgroundMusicPath :: FilePath
backgroundMusicPath = basePath </> "music.opus"

imagePath :: FilePath
imagePath = basePath </> "PNG"

mainAtlasPath :: FilePath
mainAtlasPath = imagePath </> "main-atlas.png"

playerImage :: ImageIdentifier
playerImage =
  ImageIdentifier
    {_iiAtlasPath = mainAtlasPath, _iiAtlasName = "playerShip1_blue.png"}
