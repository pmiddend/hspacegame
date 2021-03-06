{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module SG.Game where

import Apecs
  ( Entity(Entity)
  , Set
  , cmapM
  , cmapM_
  , destroy
  , get
  , modify
  , newEntity
  , runWith
  , set
  )
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
  , (.~)
  , (^.)
  , (^?!)
  , _4
  , ix
  , to
  , use
  , view
  )
import Control.Monad (replicateM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (fold, for_)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Random.Normal (normal')
import qualified Data.StateVar as SV
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((*^), (^/))
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
import SDL.Vect (V3(V3), V4(V4))
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
import SDL.Video.Renderer
  ( BlendMode(..)
  , Texture
  , clear
  , copyEx
  , fillRect
  , present
  , rendererDrawBlendMode
  , rendererDrawColor
  , textureAlphaMod
  , textureColorMod
  )
import SG.Atlas
import SG.ChunkCache
import SG.Constants
import SG.Font
import SG.LoopData
import SG.Math
import SG.SimpleLevel
import SG.Starfield
import SG.TextureCache
import SG.Time
import SG.Types
import SG.Util
import System.Random (Random, mkStdGen, randomR)

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

whenGameOver :: GameSystem () -> GameSystem ()
whenGameOver a = do
  state <- use loopGameState
  when (state == GameStateOver) a

whenGameRunning :: GameSystem () -> GameSystem ()
whenGameRunning a = do
  state <- use loopGameState
  when (state == GameStateRunning) a

eventHandler :: EventPayload -> GameSystem FinishState
eventHandler QuitEvent = pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed False (Keysym _ KC.KeycodeSpace _))) =
  continue (whenGameRunning shoot)
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

inBounds :: BodyComponent -> Bool
inBounds b =
  let largerRect = rectEmbiggen 1.5 (fromIntegral <$> gameRect)
   in rectIntersect largerRect (b ^. bodyRectangle)

updateBodies :: GameSystem ()
updateBodies = do
  timeDelta <- use loopDelta
  cmapM $ \(b@BodyComponent {}, ety) -> do
    unless (inBounds b) (destroy ety (Proxy @AllComponents))
    pure
      (b & bodyData . bodyPosition +~ timeDelta *^
       (b ^. bodyData . bodyVelocity) &
       bodyData .
       bodyAngle +~
       (timeDelta ~^ (b ^. bodyData . bodyAngularVelocity)))

newEntity' :: (Set World LoopState c) => c -> GameSystem ()
newEntity' xs = void $ newEntity xs

randomPair :: (a, a) -> GameSystem a
randomPair (x, y) = do
  index <- randomUniform @Int 0 1
  if index == 0
    then pure x
    else pure y

spawnMeteorParticle' :: TimePoint -> V2 Double -> GameSystem ()
spawnMeteorParticle' now center = do
  lifetime <- round @Double @Integer <$> randomNormal @Double 500 1500
  size <- randomUniform 8 80
  velocitySignum <- V2 <$> randomPair (-1, 1) <*> randomPair (-1, 1)
  velocityAmount <- V2 <$> randomUniform 10 300 <*> randomUniform 10 300
  let velocity = velocitySignum * velocityAmount
  angularVelocity <- randomNormal 0 5
  newEntity'
    ( Lifetime
        (milliseconds lifetime)
        (milliseconds lifetime $+ now)
        (Just (LinearFromTo 1 0))
    , ImageComponent (StillImage meteorParticleImage)
    , ColorComponent (V4 255 255 255 255)
    , BodyComponent $
      BodyData
        { _bodyPosition = center - (floatV2 (V2 size size) ^/ 2.0)
        , _bodySize = V2 size size
        , _bodyAngle = Radians 0
        , _bodyVelocity = velocity
        , _bodyAngularVelocity = Radians angularVelocity
        })

initEcs :: GameSystem ()
initEcs = do
  p <-
    newEntity
      ( BodyComponent $
        BodyData
          { _bodyPosition =
              (floatV2 gameSize ^/ 2.0) - floatV2 playerSize ^/ 2.0
          , _bodySize = playerSize
          , _bodyAngle = Radians 0
          , _bodyVelocity = V2 0 0
          , _bodyAngularVelocity = Radians 0
          }
      , ImageComponent (StillImage playerImage))
  loopPlayer .= p

shoot :: GameSystem ()
shoot = do
  currentEnergy <- use loopCurrentEnergy
  when (currentEnergy > laserEnergy) $ do
    loopCurrentEnergy -= laserEnergy
    BodyComponent bd <- use loopPlayer >>= get
    playChunk pewPath
    void $
      newEntity
        ( Bullet laserRadius laserDamage
        , BodyComponent $
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
        , ImageComponent (StillImage laserImage))

spawn :: Spawn -> GameSystem ()
spawn (Spawn _ (SpawnTypeHint td)) = do
  now <- getNow
  let dur = seconds @Integer 2
  loopHint .= Just (Hint td dur (dur $+ now))
spawn (Spawn _ (SpawnTypeMeteor meteorType pos)) =
  newEntity'
    ( Target (meteorType ^. meteorRadius) (meteorType ^. meteorHealth)
    , BodyComponent $
      BodyData
        { _bodyPosition = pos
        , _bodySize = meteorType ^. meteorSize
        , _bodyAngle = Radians 0
        , _bodyVelocity = meteorType ^. meteorVelocity
        , _bodyAngularVelocity = meteorType ^. meteorAngularVelocity
        }
    , ImageComponent (StillImage (meteorType ^. meteorImage)))

spawnEnemies :: GameSystem ()
spawnEnemies = do
  elapsed <- elapsedTime
  currentLevel <- use loopLevel
  remainingLevel <- spawnEnemies' elapsed currentLevel
  loopLevel .= remainingLevel
  where
    spawnEnemies' :: Duration -> [Spawn] -> GameSystem [Spawn]
    spawnEnemies' elapsed =
      uncurry (>>) . (mapM_ spawn *** pure) .
      span ((< elapsed) . view spawnTimeDiff)

handleCollisions :: GameSystem ()
handleCollisions = do
  BodyComponent pb <- use loopPlayer >>= get
  cmapM_ $ \(Target tr _, BodyComponent bdT) ->
    when
      (circlesIntersect
         (Circle (bdT ^. bodyCenter) tr)
         (Circle (pb ^. bodyCenter) playerRadius)) $ do
      loopGameState .= GameStateOver
      playChunk gameOverSoundPath
  cmapM_ $ \(Target tr th, BodyComponent bdT, etyT) ->
    cmapM_ $ \(Bullet br bd, BodyComponent bdB, etyB) ->
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
            let explosionDuration = explosionAnimation ^. aiTotalDuration
            newEntity'
              ( ImageComponent (DynamicImage (Animation explosionAnimation now))
              , Lifetime
                  explosionDuration
                  (explosionDuration $+ now)
                  (Just (LinearFromTo 1 0))
              , BodyComponent $
                BodyData
                  { _bodyPosition =
                      bdT ^. bodyCenter - floatV2 explosionSize ^/ 2.0
                  , _bodySize = explosionSize
                  , _bodyAngle = Radians 0
                  , _bodyVelocity = V2 0 0
                  , _bodyAngularVelocity = Radians 0
                  })
            replicateM_ 10 (spawnMeteorParticle' now (bdT ^. bodyCenter))
            playChunk explosionSoundPath
            loopScore += 1
          else do
            playChunk collisionSoundPath
            set etyT (Target tr newHealth)

calculateRamp :: TimePoint -> TimePoint -> Duration -> Maybe Ramp -> Double
calculateRamp now end _ Nothing =
  if now > end
    then -1
    else 1
calculateRamp now end dur (Just (LinearFromTo from' to')) =
  if now > end
    then -1
    else let t :: Double
             t = 1 - ((end `timeDiff` now) `durationDiv` dur)
          in lerp from' to' t
calculateRamp now end dur (Just (UpThenDown upDur downDur from' to')) =
  if now > end
    then -1
    else let t :: Double
             t = 1 - ((end `timeDiff` now) `durationDiv` dur)
          in if t < upDur
               then lerp from' to' (t / upDur)
               else if t > (1 - downDur)
                      then lerp from' to' ((1 - t) / downDur)
                      else to'

deleteRetirees :: GameSystem ()
deleteRetirees = do
  now <- getNow
  cmapM_ $ \(Lifetime dur ltEnd ramp, ety) -> do
    let lifetimePercent = calculateRamp now ltEnd dur ramp
    if lifetimePercent <= 0
      then destroyEntity ety
      else let modColor (Just (ColorComponent (V4 r g b _))) =
                 Just
                   (ColorComponent (V4 r g b (round (lifetimePercent * 255))))
               modColor Nothing = Nothing
            in modify ety modColor

updateHint :: GameSystem ()
updateHint = do
  now <- getNow
  loopHint %= filterMaybe ((> now) . view hintEnd)

mainLoop :: GameSystem ()
mainLoop = do
  beforeFrame <- getNow
  events <- liftIO pollEvents
  eventResult <- fold <$> mapM eventHandler (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw
      whenGameRunning $ do
        updatePlayer
        deleteRetirees
        updateBodies
        spawnEnemies
        replenishEnergy
        handleCollisions
      afterFrame <- getNow
      let timeDiffSecs :: Double
          timeDiffSecs =
            (afterFrame `timeDiff` beforeFrame) `durationDiv` seconds @Integer 1
      loopDelta .= timeDiffSecs
      delta <- use loopDelta
      loopStarfield %= stepStarfield delta
      updateHint
      mainLoop

updatePlayer :: GameSystem ()
updatePlayer = do
  timeDelta <- use loopDelta
  keys <- use loopPlayerKeys
  pe <- use loopPlayer
  modify pe $ \(BodyComponent b) ->
    BodyComponent (b & bodyVelocity %~ updatePlayerVelocity timeDelta keys)

determineAnimFrame :: TimePoint -> Animation -> Int
determineAnimFrame now anim =
  let elapsed :: Duration
      elapsed = now `timeDiff` (anim ^. animationStart)
      dur :: Duration
      dur = anim ^. animationIdentifier . aiFrameDuration
      frameCount = anim ^. animationIdentifier . aiFrameCount
   in fromIntegral (elapsed `durationDivInt` dur) `mod` frameCount

determineAnimRect :: Int -> Animation -> SizedTexture -> Rectangle Int
determineAnimRect frame anim texture =
  let frameSize = anim ^. animationIdentifier . aiFrameSize
      perRow = (texture ^. stSize . _x) `div` (frameSize ^. _x)
      (row, column) = frame `divMod` perRow
   in Rectangle (V2 column row * frameSize) frameSize

withNewTextureColor :: Maybe Color -> Texture -> GameSystem a -> GameSystem a
withNewTextureColor c t f = do
  let V4 r g b a = fromMaybe (V4 255 255 255 255) c
  let texColorMod = textureColorMod t
      texAlphaMod = textureAlphaMod t
  colorBefore <- SV.get texColorMod
  alphaBefore <- SV.get texAlphaMod
  texColorMod SV.$= V3 r g b
  texAlphaMod SV.$= a
  result <- f
  texColorMod SV.$= colorBefore
  texAlphaMod SV.$= alphaBefore
  pure result

withNewDrawColor :: Maybe Color -> GameSystem a -> GameSystem a
withNewDrawColor c f = do
  let realColor = fromMaybe (V4 255 255 255 255) c
  renderer <- use loopRenderer
  let drawColor = rendererDrawColor renderer
  colorBefore <- SV.get drawColor
  drawColor SV.$= realColor
  result <- f
  drawColor SV.$= colorBefore
  pure result

fillRectColor :: Rectangle Int -> Color -> GameSystem ()
fillRectColor r c = do
  renderer <- use loopRenderer
  withNewDrawColor
    (Just c)
    (fillRect renderer (Just (r ^. to (fromIntegral <$>) . sdlRect)))

loadTextCached' :: TextDescriptor -> GameSystem SizedTexture
loadTextCached' rt = do
  fontCache <- use loopFontCache
  renderer <- use loopRenderer
  textCache <- use loopTextCache
  loadTextCached renderer fontCache textCache rt

textSize :: TextDescriptor -> GameSystem (V2 Int)
textSize rt = view stSize <$> loadTextCached' rt

drawText :: V2 Int -> TextDescriptor -> GameSystem ()
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

randomUniform :: (Random a) => a -> a -> GameSystem a
randomUniform a b = do
  (v, rng') <- randomR (a, b) <$> use loopRng
  loopRng .= rng'
  pure v

randomNormal :: (Random a, Floating a) => a -> a -> GameSystem a
randomNormal mean stddev = do
  (v, rng') <- normal' (mean, stddev) <$> use loopRng
  loopRng .= rng'
  pure v

drawScore :: GameSystem ()
drawScore = do
  currentScore <- use (loopScore . score)
  let label =
        TextDescriptor
          { _rtFontDescriptor = hudFont
          , _rtColor = V4 200 200 200 255
          , _rtText = "Score: " <> textShow currentScore
          }
  labelSize <- textSize label
  drawText
    (V2 (gameSize ^. _x - labelSize ^. _x - hudMargin ^. _x) (hudMargin ^. _y))
    label

drawCenteredIn :: Rectangle Int -> TextDescriptor -> GameSystem ()
drawCenteredIn r label = do
  labelSize <- textSize label
  drawText
    (round <$>
     (floatV2 (r ^. rectPos) + (r ^. rectSize . floatingV2) ^/ 2 -
      floatV2 @Int @Double labelSize ^/
      2))
    label

drawCentered :: TextDescriptor -> GameSystem ()
drawCentered = drawCenteredIn gameRect

drawEnergy :: GameSystem ()
drawEnergy = do
  let label =
        TextDescriptor
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
  let drawBody ::
           SizedTexture
        -> Rectangle Int
        -> BodyData
        -> Maybe ColorComponent
        -> GameSystem ()
      drawBody tex atlasRect bd color =
        withNewTextureColor (view entityColor <$> color) (tex ^. stTexture) $
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
  cmapM_ $ \(BodyComponent bd, ImageComponent i, color) ->
    case i of
      StillImage (ImageIdentifier atlasPath atlasName) -> do
        foundAtlas <- loadAtlasCached atlasCache atlasPath
        drawBody
          (foundAtlas ^. atlasTexture)
          (foundAtlas ^?! atlasFrames . ix atlasName)
          bd
          color
      DynamicImage a -> do
        animationTexture <-
          loadTextureCached
            textureCache
            (a ^. animationIdentifier . aiAtlasPath)
        drawBody
          animationTexture
          (determineAnimRect (determineAnimFrame now a) a animationTexture)
          bd
          color
  drawEnergy
  drawScore
  drawHint
  whenGameOver $
    drawCentered
      (TextDescriptor
         { _rtFontDescriptor = announceFont
         , _rtColor = V4 255 255 255 255
         , _rtText = "Game Over"
         })
  liftIO (present renderer)

drawHint :: GameSystem ()
drawHint = do
  h <- use loopHint
  now <- getNow
  for_ h $ \hint -> do
    let height = (gameSize ^. _y) `div` 5
        hintRect :: Rectangle Int
        hintRect =
          Rectangle
            (V2 0 ((gameSize ^. _y) - height))
            (V2 (gameSize ^. _x) height)
        (V4 r g b _) = hintBackgroundColor
        ma =
          round
            @Double
            (calculateRamp
               now
               (hint ^. hintEnd)
               (hint ^. hintDuration)
               (Just (UpThenDown 0.2 0.2 0 128)))
        maf =
          round
            @Double
            (calculateRamp
               now
               (hint ^. hintEnd)
               (hint ^. hintDuration)
               (Just (UpThenDown 0.2 0.2 0 255)))
        modulatedColor = V4 r g b ma
    fillRectColor hintRect modulatedColor
    drawCenteredIn hintRect (hint ^. hintText & rtColor . _4 .~ maf)

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
                rendererLogicalSize renderer SV.$=
                  Just (fromIntegral <$> gameSize)
                rendererDrawBlendMode renderer SV.$= BlendAlphaBlend
                w <- initWorld
                gameStart <- getNow
                let initialLoopData =
                      LoopData
                        { _loopTextureCache = textureCache
                        , _loopAtlasCache = atlasCache
                        , _loopChunkCache = chunkCache
                        , _loopRenderer = renderer
                        , _loopRng = mkStdGen 0
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
                        , _loopGameState = GameStateRunning
                        , _loopPlayer = Entity (-1)
                        , _loopHint = Nothing
                        }
                runLoop initialLoopData (runWith w (initEcs >> mainLoop))
