{-# LANGUAGE OverloadedStrings #-}

module SG.Constants where

import Data.Time.Units (Millisecond)
import Linear.V2 (V2(V2))
import SG.Math
import SG.Types
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

asteroidMediumSize :: V2 Int
asteroidMediumSize = V2 42 42

asteroidMediumRadius :: Double
asteroidMediumRadius = 38.0

asteroidMediumHealth :: Health
asteroidMediumHealth = 100

laserDamage :: Health
laserDamage = 50

asteroidVelocity :: V2 Double
asteroidVelocity = V2 0 200

asteroidAngularVelocity :: Radians
asteroidAngularVelocity = Radians 0.3

laserSize :: V2 Int
laserSize = V2 4 26

laserRadius :: Double
laserRadius = 24.0

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

pewPath :: FilePath
pewPath = basePath </> "Bonus" </> "sfx_laser1.wav"

explosionSoundPath :: FilePath
explosionSoundPath = basePath </> "explosion-short.wav"

playerImage :: ImageIdentifier
playerImage = ImageIdentifier mainAtlasPath "playerShip1_blue.png"

laserImage :: ImageIdentifier
laserImage = ImageIdentifier mainAtlasPath "laserBlue01.png"

asteroidMediumImage :: ImageIdentifier
asteroidMediumImage = ImageIdentifier mainAtlasPath "meteorBrown_med1.png"

explosionSize :: V2 Int
explosionSize = V2 32 32

explosionAnimation :: AnimationIdentifier
explosionAnimation =
  AnimationIdentifier
    { _aiAtlasPath = imagePath </> "explosion.png"
    , _aiFrameCount = 32
    , _aiFrameSize = V2 64 64
    , _aiFrameDuration = 30
    }
