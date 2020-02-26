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
  , (-=)
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
import Data.Proxy
import Data.StateVar (($=), get)
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
  , InputMotion(Pressed)
  , KeyboardEventData(KeyboardEventData)
  , eventPayload
  , pollEvents
  )
import qualified SDL.Font as Font
import SDL.Init (InitFlag(InitAudio, InitVideo), initialize, quit)
import SDL.Input.Keyboard (Keysym(Keysym))
import qualified SDL.Input.Keyboard.Codes as KC
import SDL.Mixer (withAudio)
import SDL.Vect (V4(V4))
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
import SDL.Video.Renderer (clear, copyEx, fillRect, present, rendererDrawColor)
import SG.Atlas
import SG.ChunkCache
import SG.Constants
import SG.Font
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
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym _ KC.KeycodeSpace _))) =
  continue shoot
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

shoot :: GameSystem ()
shoot = do
  currentEnergy <- use loopCurrentEnergy
  when (currentEnergy > laserEnergy) $ do
    loopCurrentEnergy -= laserEnergy
    cmapM_ $ \(Player, Body bd) -> do
      void $
        newEntity
          ( Bullet laserRadius laserDamage
          , Body $
            BodyData
              { _bodyPosition =
                  V2
                    (bd ^. bodyPosition . _x +
                     fromIntegral (bd ^. bodySize . _x) /
                     2.0)
                    (bd ^. bodyPosition . _y)
              , _bodySize = laserSize
              , _bodyAngle = Radians 0
              , _bodyVelocity = laserSpeed
              , _bodyAngularVelocity = Radians 0
              }
          , Image laserImage)
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
      replenishEnergy
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

fillRectColor :: Rectangle Int -> Color -> GameSystem ()
fillRectColor r c = do
  renderer <- use loopRenderer
  let drawColor = rendererDrawColor renderer
  colorBefore <- get drawColor
  drawColor $= c
  fillRect renderer (Just (r ^. to (fromIntegral <$>) . sdlRect))
  drawColor $= colorBefore

loadTextCached' :: RenderedText -> GameSystem SizedTexture
loadTextCached' rt = do
  fontCache <- use loopFontCache
  renderer <- use loopRenderer
  textCache <- use loopTextCache
  loadTextCached renderer fontCache textCache rt

textSize :: RenderedText -> GameSystem (V2 Int)
textSize rt = view stSize <$> loadTextCached' rt

drawText :: V2 Int -> RenderedText -> GameSystem ()
drawText position rt = do
  st <- loadTextCached' rt
  renderer <- use loopRenderer
  copyEx
    renderer
    (st ^. stTexture)
    Nothing
    (Just (Rectangle position (st ^. stSize) ^. to (fromIntegral <$>) . sdlRect))
    0
    Nothing
    (V2 False False)

drawScore :: GameSystem ()
drawScore = do
  currentScore <- use (loopScore . score)
  let label =
        RenderedText
          { _rtFontDescriptor = hudFont
          , _rtColor = V4 200 200 200 255
          , _rtText = "Score: " <> textShow currentScore
          }
  labelSize <- textSize label
  drawText
    (V2 (gameSize ^. _x - labelSize ^. _x - hudMargin ^. _x) (hudMargin ^. _y))
    label

drawEnergy :: GameSystem ()
drawEnergy = do
  let label =
        RenderedText
          { _rtFontDescriptor = hudFont
          , _rtColor = V4 255 255 255 255
          , _rtText = "Energy:"
          }
  labelSize <- textSize label
  let energyWidth = 200
      energyHeight = 20
      energyPos =
        V2
          ((hudMargin ^. _x) + (hudMargin ^. _x) + labelSize ^. _x)
          (hudMargin ^. _y)
  currentEnergy <- use loopCurrentEnergy
  maxEnergy <- use loopMaxEnergy
  drawText hudMargin label
  fillRectColor
    (Rectangle energyPos (V2 energyWidth energyHeight))
    (V4 255 0 0 255)
  fillRectColor
    (Rectangle
       energyPos
       (V2
          (round
             ((currentEnergy * Energy (fromIntegral energyWidth)) / maxEnergy))
          energyHeight))
    (V4 0 255 0 255)

replenishEnergy :: GameSystem ()
replenishEnergy = do
  currentEnergy <- use loopCurrentEnergy
  maxEnergy <- use loopMaxEnergy
  delta <- use loopDelta
  loopCurrentEnergy .=
    min maxEnergy (currentEnergy + Energy delta * energyReplenishPerSecond)

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
  drawEnergy
  drawScore
  liftIO (present renderer)

gameMain :: IO ()
gameMain =
  bracket_ (initialize [InitVideo, InitAudio]) quit $
  bracket (createWindow "hspacegame" windowConfig) destroyWindow $ \window ->
    bracket (createRenderer window (-1) rendererConfig) destroyRenderer $ \renderer ->
      bracket (initTextureCache renderer) destroyTextureCache $ \textureCache ->
        bracket (initAtlasCache textureCache) destroyAtlasCache $ \atlasCache ->
          bracket_ Font.initialize Font.quit $
          bracket initFontCache destroyFontCache $ \fontCache ->
            bracket initTextCache destroyTextCache $ \textCache ->
              withAudio def 1024 $ bracket initChunkCache destroyChunkCache $ \chunkCache -> do
                rendererLogicalSize renderer $= Just (fromIntegral <$> gameSize)
                w <- initWorld
                gameStart <- getNow
                let initialLoopData =
                      LoopData
                        { _loopTextureCache = textureCache
                        , _loopAtlasCache = atlasCache
                        , _loopChunkCache = chunkCache
                        , _loopRenderer = renderer
                        , _loopTextCache = textCache
                        , _loopFontCache = fontCache
                        , _loopDelta = 0
                        , _loopWorld = w
                        , _loopStarfield = initStarfield (mkStdGen 0)
                        , _loopPlayerKeys = initialPlayerDirection
                        , _loopLevel = simpleLevel
                        , _loopGameStart = gameStart
                        , _loopScore = Score 0
                        , _loopCurrentEnergy = Energy 50
                        , _loopMaxEnergy = initialMaxEnergy
                        }
                runLoop initialLoopData (runWith w (initEcs >> mainLoop))
