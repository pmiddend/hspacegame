{-# LANGUAGE OverloadedStrings #-}

module SG.Constants where

import Data.Time.Units (Millisecond)
import Linear.V2 (V2(V2))
import SDL.Vect (V4(V4))
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
asteroidMediumRadius = 18.0

asteroidMediumHealth :: Health
asteroidMediumHealth = 100

laserDamage :: Health
laserDamage = 50

asteroidVelocity :: V2 Double
asteroidVelocity = V2 0 200

asteroidAngularVelocity :: Radians
asteroidAngularVelocity = Radians 1

laserSize :: V2 Int
laserSize = V2 4 26

laserRadius :: Double
laserRadius = 13.0

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

collisionSoundPath :: FilePath
collisionSoundPath = basePath </> "collision.wav"

gameOverSoundPath :: FilePath
gameOverSoundPath = basePath </> "gameover.wav"

playerImage :: ImageIdentifier
playerImage = ImageIdentifier mainAtlasPath "playerShip1_blue.png"

laserImage :: ImageIdentifier
laserImage = ImageIdentifier mainAtlasPath "laserBlue01.png"

asteroidMediumImage :: ImageIdentifier
asteroidMediumImage = ImageIdentifier mainAtlasPath "meteorBrown_med1.png"

explosionSize :: V2 Int
explosionSize = V2 100 100

meteorParticleLifetime :: Millisecond
meteorParticleLifetime = 1000

meteorParticleSize :: V2 Int
meteorParticleSize = V2 16 16

meteorParticleImage :: ImageIdentifier
meteorParticleImage = ImageIdentifier mainAtlasPath "meteorBrown_tiny1.png"

explosionAnimation :: AnimationIdentifier
explosionAnimation =
  AnimationIdentifier
    { _aiAtlasPath = imagePath </> "explosion.png"
    , _aiFrameCount = 32
    , _aiFrameSize = V2 64 64
    , _aiFrameDuration = 30
    }

initialMaxEnergy :: Energy
initialMaxEnergy = Energy 100

hudFont :: FontDescriptor
hudFont =
  FontDescriptor
    {_fdFont = basePath </> "Bonus" </> "kenvector_future.ttf", _fdSize = 15}

announceFont :: FontDescriptor
announceFont =
  FontDescriptor
    {_fdFont = basePath </> "Bonus" </> "kenvector_future.ttf", _fdSize = 65}

laserEnergy :: Energy
laserEnergy = Energy 80

energyReplenishPerSecond :: Energy
energyReplenishPerSecond = Energy 60

hudMargin :: V2 Int
hudMargin = V2 5 5

playerRadius :: Double
playerRadius = 14.0

hintBackgroundColor :: Color
hintBackgroundColor = V4 64 64 64 128

hintTextColor :: Color
hintTextColor = V4 255 255 255 255
