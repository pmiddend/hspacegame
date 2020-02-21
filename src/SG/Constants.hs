{-# LANGUAGE OverloadedStrings #-}

module SG.Constants where

import Linear.V2 (V2(V2))
import SG.Types (ImageIdentifier(..))
import System.FilePath ((</>))

gameSize :: V2 Int
gameSize = V2 1024 768

playerSize :: V2 Int
playerSize = V2 50 32

basePath :: FilePath
basePath = "data"

imagePath :: FilePath
imagePath = basePath </> "PNG"

mainAtlasPath :: FilePath
mainAtlasPath = imagePath </> "main-atlas.png"

playerImage :: ImageIdentifier
playerImage =
  ImageIdentifier
    {_iiAtlasPath = mainAtlasPath, _iiAtlasName = "playerShip1_blue.png"}
