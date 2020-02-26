{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module SG.Game where

import Apecs (Set, cmap, cmapM, cmapM_, destroy, newEntity, runWith, set)
import Control.Arrow ((***))
import Control.Exception (bracket, bracket_)
import Control.Lens
  ( (%=)
  , (%~)
  , (&)
  , (+=)
  , (+~)
  , (.=)
  , (^.)
  , (^?!)
  , ix
  , to
  , use
  , view
  )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (fold, for_)
import Data.Monoid (All(All), getAll)
import Data.Proxy
import Data.StateVar (($=))
import Data.Time.Units
  ( Microsecond
  , Millisecond
  , TimeUnit
  , convertUnit
  , toMicroseconds
  )
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((*^), (^/))
import Numeric.Lens (negated)
import Prelude hiding (lookup)
import SDL.Event
  ( EventPayload(KeyboardEvent, QuitEvent)
  , InputMotion(Pressed, Released)
  , KeyboardEventData(KeyboardEventData)
  , eventPayload
  , pollEvents
  )
import SDL.Init (InitFlag(InitAudio, InitVideo), initialize, quit)
import SDL.Input.Keyboard (Keysym(Keysym))
import qualified SDL.Input.Keyboard.Codes as KC
import SDL.Mixer (withAudio)
import SDL.Video
  ( RendererConfig
  , WindowConfig(windowInitialSize, windowResizable)
  , createRenderer
  , createWindow
  , defaultRenderer
  , defaultWindow
  , destroyRenderer
  , destroyWindow
  , rendererLogicalSize
  )
import SDL.Video.Renderer (clear, copyEx, present)
import SG.Atlas
import SG.ChunkCache
import SG.Constants
import SG.LoopData
import SG.Math
import SG.Starfield
import SG.TextureCache
import SG.Types
import SG.Util
import System.Random (mkStdGen)

windowConfig :: WindowConfig
windowConfig =
  defaultWindow
    {windowResizable = True, windowInitialSize = fromIntegral <$> gameSize}

rendererConfig :: RendererConfig
rendererConfig = defaultRenderer

data FinishState
  = Finished
  | NotFinished

instance Semigroup FinishState where
  Finished <> _ = Finished
  _ <> Finished = Finished
  _ <> _ = NotFinished

instance Monoid FinishState where
  mempty = NotFinished

keyCodeToPlayerDirection :: Num a => InputMotion -> KC.Keycode -> Maybe (V2 a)
keyCodeToPlayerDirection c KC.KeycodeA =
  Just $
  V2
    (if c == Pressed
       then -1
       else 1)
    0
keyCodeToPlayerDirection c KC.KeycodeD =
  Just $
  V2
    (if c == Pressed
       then 1
       else -1)
    0
keyCodeToPlayerDirection c KC.KeycodeW =
  Just $
  V2
    0
    (if c == Pressed
       then -1
       else 1)
keyCodeToPlayerDirection c KC.KeycodeS =
  Just $
  V2
    0
    (if c == Pressed
       then 1
       else -1)
keyCodeToPlayerDirection _ _ = Nothing

eventHandler :: EventPayload -> GameSystem FinishState
eventHandler QuitEvent = pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ spaceStatus False (Keysym _ KC.KeycodeSpace _))) =
  continue $ do
    loopSpacePressed .= spaceStatus
    loopLastShot %= resetIf (spaceStatus == Pressed)
eventHandler (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ kc _))) =
  continue $ for_ (keyCodeToPlayerDirection keyState kc) (loopPlayerKeys +=)
eventHandler _ = continue (pure ())

class Applicator a where
  applicate :: ((forall c. a c -> c) -> b -> b) -> a b -> a b

instance Applicator V2 where
  applicate f (V2 x y) = V2 (f (view _x) x) (f (view _y) y)

updatePlayerVelocity :: Double -> PlayerKeys -> Endo (V2 Double)
updatePlayerVelocity timeDelta pk v =
  let updatePlayerVelocity' :: (forall a. V2 a -> a) -> Double -> Double
      updatePlayerVelocity' getter cv =
        let velocitySignum = getter pk
            inverseSignum = (-1) * signum cv
            playerFrictionPart = abs cv / getter playerMaxVelocity
         in if velocitySignum /= 0
              then fromIntegral velocitySignum * getter playerMaxVelocity
              else if abs cv < 0.000001
                     then 0
                     else cv + inverseSignum * timeDelta * playerFrictionPart *
                          getter playerFriction
   in applicate updatePlayerVelocity' v

inBounds :: Body -> Bool
inBounds b =
  let largerRect = rectEmbiggen 1.5 (fromIntegral <$> gameRect)
   in rectIntersect largerRect (b ^. bodyRectangle)

updateBodies :: GameSystem ()
updateBodies = do
  timeDelta <- use loopDelta
  cmapM $ \(b@Body {}, ety) -> do
    unless (inBounds b) (destroy ety (Proxy @AllComponents))
    pure
      (b & bodyData . bodyPosition +~ timeDelta *^
       (b ^. bodyData . bodyVelocity) &
       bodyData .
       bodyAngle +~
       (timeDelta ~^ (b ^. bodyData . bodyAngularVelocity)))

newEntity' :: (Set World LoopState c) => c -> GameSystem ()
newEntity' xs = void $ newEntity xs

initEcs :: GameSystem ()
initEcs =
  newEntity'
    ( Player
    , Body $
      BodyData
        { _bodyPosition =
            ((fromIntegral <$> gameSize) ^/ 2.0) - (fromIntegral <$> playerSize) ^/
            2.0
        , _bodySize = playerSize
        , _bodyAngle = Radians 0
        , _bodyVelocity = V2 0 0
        , _bodyAngularVelocity = Radians 0
        }
    , Image playerImage)

maybeShoot :: GameSystem ()
maybeShoot = do
  spacePressed <- use loopSpacePressed
  lastShot <- use loopLastShot
  now <- getNow
  when
    (spacePressed == Pressed &&
     getAll
       (foldMap (All . (> playerShootingFrequency) . (now `timeDiff`)) lastShot)) $
    cmapM_ $ \(Player, Body bd) -> do
    void $
      newEntity
        ( Bullet laserRadius laserDamage
        , Body $
          BodyData
            { _bodyPosition =
                V2
                  (bd ^. bodyPosition . _x + fromIntegral (bd ^. bodySize . _x) /
                   2.0)
                  (bd ^. bodyPosition . _y)
            , _bodySize = laserSize
            , _bodyAngle = Radians 0
            , _bodyVelocity = laserSpeed
            , _bodyAngularVelocity = Radians 0
            }
        , Image laserImage)
    loopLastShot .= Just now
    playChunk pewPath

simpleLevel :: Level
simpleLevel =
  [ Spawn
      { _spawnTimeDiff = 5000 :: Millisecond
      , _spawnType = SpawnTypeAsteroidMedium
      , _spawnPosition =
          V2 500 (fromIntegral (asteroidMediumSize ^. _y . negated))
      }
  ]

spawn :: Spawn -> GameSystem ()
spawn s =
  newEntity'
    ( Target asteroidMediumRadius asteroidMediumHealth
    , Body $
      BodyData
        { _bodyPosition = s ^. spawnPosition
        , _bodySize = asteroidMediumSize
        , _bodyAngle = Radians 0
        , _bodyVelocity = asteroidVelocity
        , _bodyAngularVelocity = asteroidAngularVelocity
        }
    , Image asteroidMediumImage)

spawnEnemies :: GameSystem ()
spawnEnemies = do
  elapsed <- elapsedTime
  currentLevel <- use loopLevel
  remainingLevel <- spawnEnemies' elapsed currentLevel
  loopLevel .= remainingLevel
  where
    spawnEnemies' :: TimeUnit a => a -> [Spawn] -> GameSystem [Spawn]
    spawnEnemies' elapsed =
      uncurry (>>) . (mapM_ spawn *** pure) .
      span ((< convertUnit elapsed) . view spawnTimeDiff)

handleCollisions :: GameSystem ()
handleCollisions =
  cmapM_ $ \(Target tr th, Body bdT, etyT) ->
    cmapM_ $ \(Bullet br bd, Body bdB, etyB) ->
      when
        (circlesIntersect
           (Circle (bdT ^. bodyCenter) tr)
           (Circle (bdB ^. bodyCenter) br)) $ do
        destroy etyB (Proxy @AllComponents)
        let newHealth = th - bd
        if newHealth <= 0
          then do
            destroy etyT (Proxy @AllComponents)
            now <- getNow
            newEntity'
              ( Animation explosionAnimation now
              , Lifetime ((explosionAnimation ^. aiTotalDuration) ~^ now)
              , Body $
                BodyData
                  { _bodyPosition =
                      bdT ^. bodyCenter - (fromIntegral <$> explosionSize) ^/
                      2.0
                  , _bodySize = explosionSize
                  , _bodyAngle = Radians 0
                  , _bodyVelocity = V2 0 0
                  , _bodyAngularVelocity = Radians 0
                  })
            playChunk explosionSoundPath
            loopScore += 1
          else set etyT (Target tr newHealth)

deleteRetirees :: GameSystem ()
deleteRetirees = do
  now <- getNow
  cmapM_ $ \(Lifetime ltEnd, ety) -> when (now > ltEnd) (destroyEntity ety)

mainLoop :: GameSystem ()
mainLoop = do
  beforeFrame <- getNow
  events <- liftIO pollEvents
  eventResult <- fold <$> mapM eventHandler (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw
      updatePlayer
      deleteRetirees
      updateBodies
      spawnEnemies
      maybeShoot
      handleCollisions
      afterFrame <- getNow
      let timeDiffSecs =
            fromIntegral
              (toMicroseconds (afterFrame `timeDiff` beforeFrame :: Microsecond)) /
            1000000.0
      loopDelta .= timeDiffSecs
      delta <- use loopDelta
      loopStarfield %= stepStarfield delta
      mainLoop

updatePlayer :: GameSystem ()
updatePlayer = do
  timeDelta <- use loopDelta
  keys <- use loopPlayerKeys
  cmap $ \(Player, Body b) ->
    Body (b & bodyVelocity %~ updatePlayerVelocity timeDelta keys)

determineAnimFrame :: TimePoint -> Animation -> Int
determineAnimFrame now anim =
  let elapsed :: Microsecond
      elapsed = now `timeDiff` (anim ^. animationStart)
      duration :: Millisecond
      duration = anim ^. animationIdentifier . aiFrameDuration
      frameCount = anim ^. animationIdentifier . aiFrameCount
   in fromIntegral (toMicroseconds elapsed `div` toMicroseconds duration) `mod`
      frameCount

determineAnimRect :: Int -> Animation -> SizedTexture -> Rectangle Int
determineAnimRect frame anim texture =
  let frameSize = anim ^. animationIdentifier . aiFrameSize
      perRow = (texture ^. stSize . _x) `div` (frameSize ^. _x)
      (row, column) = frame `divMod` perRow
   in Rectangle (V2 column row * frameSize) frameSize

draw :: GameSystem ()
draw = do
  renderer <- use loopRenderer
  liftIO (clear renderer)
  atlasCache <- use loopAtlasCache
  textureCache <- use loopTextureCache
  starfield <- use loopStarfield
  drawStarfield renderer atlasCache starfield
  now <- getNow
  let drawBody tex atlasRect bd =
        copyEx
          renderer
          (tex ^. stTexture)
          (Just (atlasRect ^. to (fromIntegral <$>) . sdlRect))
          (Just
             (Rectangle
                (round <$> (bd ^. bodyPosition))
                (fromIntegral <$> (bd ^. bodySize)) ^.
              sdlRect))
          (bd ^. bodyAngle . degrees . getDegrees . to realToFrac)
          Nothing
          (V2 False False)
  cmapM_ $ \(Body bd, a@Animation {}) -> do
    animationTexture <-
      loadTextureCached textureCache (a ^. animationIdentifier . aiAtlasPath)
    drawBody
      animationTexture
      (determineAnimRect (determineAnimFrame now a) a animationTexture)
      bd
  cmapM_ $ \(Body bd, Image (ImageIdentifier atlasPath atlasName)) -> do
    foundAtlas <- loadAtlasCached atlasCache atlasPath
    drawBody
      (foundAtlas ^. atlasTexture)
      (foundAtlas ^?! atlasFrames . ix atlasName)
      bd
  liftIO (present renderer)

gameMain :: IO ()
gameMain =
  bracket_ (initialize [InitVideo, InitAudio]) quit $
  bracket (createWindow "hspacegame" windowConfig) destroyWindow $ \window ->
    bracket (createRenderer window (-1) rendererConfig) destroyRenderer $ \renderer ->
      bracket (initTextureCache renderer) destroyTextureCache $ \textureCache ->
        bracket (initAtlasCache textureCache) destroyAtlasCache $ \atlasCache ->
          withAudio def 1024 $ bracket initChunkCache destroyChunkCache $ \chunkCache -> do
            rendererLogicalSize renderer $= Just (fromIntegral <$> gameSize)
            w <- initWorld
            gameStart <- getNow
            let initialLoopData =
                  LoopData
                    textureCache
                    atlasCache
                    chunkCache
                    renderer
                    0
                    w
                    (initStarfield (mkStdGen 0))
                    initialPlayerDirection
                    Released
                    Nothing
                    simpleLevel
                    gameStart
                    0
            runLoop initialLoopData (runWith w (initEcs >> mainLoop))
