{-# LANGUAGE OverloadedStrings #-}

module SG.Constants where

import Data.Time.Units (Millisecond)
import Linear.V2 (V2(V2))
import SG.Math (Rectangle(Rectangle))
import SG.Types (ImageIdentifier(..))
import System.FilePath ((</>))

playerShootingFrequency :: Millisecond
playerShootingFrequency = 500 :: Millisecond

gameSize :: V2 Int
gameSize = V2 1024 768

playerMaxVelocity :: V2 Double
playerMaxVelocity = V2 200 200

playerFriction :: V2 Double
playerFriction = V2 300 300

gameRect :: Rectangle Int
gameRect = Rectangle (V2 0 0) gameSize

playerSize :: V2 Int
playerSize = V2 50 32

laserSize :: V2 Int
laserSize = V2 4 26

laserSpeed :: V2 Double
laserSpeed = V2 0 (-300)

basePath :: FilePath
basePath = "data"

backgroundMusicPath :: FilePath
backgroundMusicPath = basePath </> "music.opus"

imagePath :: FilePath
imagePath = basePath </> "PNG"

mainAtlasPath :: FilePath
mainAtlasPath = imagePath </> "main-atlas.png"

playerImage :: ImageIdentifier
playerImage = ImageIdentifier mainAtlasPath "playerShip1_blue.png"

laserImage :: ImageIdentifier
laserImage = ImageIdentifier mainAtlasPath "laserBlue01.png"
