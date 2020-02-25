{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Game where

import Apecs (SystemT, cmap, cmapM, cmapM_, destroy, newEntity, runWith)
import Control.Arrow ((***))
import Control.Exception (bracket, bracket_)
import Control.Exception.Lifted (catch)
import Control.Lens
  ( Getting
  , (%=)
  , (&)
  , (+=)
  , (+~)
  , (.=)
  , (^.)
  , (^?!)
  , ix
  , makeLenses
  , to
  , use
  , view
  )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
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
import SDL.Exception (SDLException)
import SDL.Init (InitFlag(InitAudio, InitVideo), initialize, quit)
import SDL.Input.Keyboard (Keysym(Keysym))
import qualified SDL.Input.Keyboard.Codes as KC
import SDL.Mixer
  ( Chunk
  , pattern Forever
  , free
  , load
  , play
  , playMusic
  , withAudio
  )
import SDL.Video
  ( Renderer
  , RendererConfig
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
import SG.Math
import SG.Starfield
import SG.TextureCache
import SG.Types
import System.Random (mkStdGen)

type PlayerKeys = V2 Int

data LoopData =
  LoopData
    { _loopTextureCache :: TextureCache
    , _loopAtlasCache :: AtlasCache
    , _loopChunkCache :: ChunkCache
    , _loopRenderer :: Renderer
    , _loopDelta :: Double
    , _loopWorld :: World
    , _loopStarfield :: Starfield
    , _loopPlayerKeys :: PlayerKeys
    , _loopSpacePressed :: InputMotion
    , _loopLastShot :: Maybe TimePoint
    , _loopLevel :: Level
    , _loopGameStart :: TimePoint
    }

makeLenses ''LoopData

--type GameSystem a = StateT LoopData (SystemT World IO) a
type LoopState = StateT LoopData IO

type GameSystem = SystemT World LoopState

instance MonadState s m => MonadState s (SystemT w m) where
  get = lift get
  put s = lift (put s)

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

continue :: (Monad m, Monoid b) => m a -> m b
continue x = x >> pure mempty

resetIf :: Bool -> Maybe a -> Maybe a
resetIf True _ = Nothing
resetIf _ x = x

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
    pure (b & bodyPosition +~ timeDelta *^ (b ^. bodyVelocity))

initEcs :: GameSystem ()
initEcs =
  void $
  newEntity
    ( Player
    , Body
        { _bodyPosition =
            ((fromIntegral <$> gameSize) ^/ 2.0) - (fromIntegral <$> playerSize) ^/
            2.0
        , _bodySize = playerSize
        , _bodyAngle = Radians 0
        , _bodyVelocity = V2 0 0
        , _bodyAngularVelocity = Radians 0
        }
    , Image playerImage)

playCatchIO :: MonadIO m => Chunk -> m ()
playCatchIO c =
  liftIO
    (play c `catch` \e ->
       putStrLn ("play sound failed" <> show (e :: SDLException)))

playChunk :: MonadIO m => ChunkCache -> FilePath -> m ()
playChunk cache fp = do
  c <- loadChunk cache fp
  playCatchIO c

maybeShoot :: GameSystem ()
maybeShoot = do
  spacePressed <- use loopSpacePressed
  lastShot <- use loopLastShot
  chunkCache <- use loopChunkCache
  now <- getNow
  when
    (spacePressed == Pressed &&
     getAll
       (foldMap (All . (> playerShootingFrequency) . (now `timeDiff`)) lastShot)) $
    cmapM_ $ \(Player, Body {_bodyPosition = playerPos, _bodySize = playerSize'}) -> do
    void $
      newEntity
        ( Bullet
        , Body
            { _bodyPosition =
                V2
                  (playerPos ^. _x + fromIntegral (playerSize' ^. _x) / 2.0)
                  (playerPos ^. _y)
            , _bodySize = laserSize
            , _bodyAngle = Radians 0
            , _bodyVelocity = laserSpeed
            , _bodyAngularVelocity = Radians 0
            }
        , Image laserImage)
    loopLastShot .= Just now
    playChunk chunkCache pewPath

simpleLevel :: Level
simpleLevel =
  [ Spawn
      { _spawnTimeDiff = 5000 :: Millisecond
      , _spawnType = SpawnTypeAsteroidMedium
      , _spawnPosition =
          V2 500 (fromIntegral (asteroidMediumSize ^. _y . negated))
      }
  ]

(^!) :: MonadIO m => m s -> Getting a s a -> m a
a ^! b = view b <$> a

spawn :: Spawn -> GameSystem ()
spawn s =
  void $
  newEntity
    ( Target
    , Body
        { _bodyPosition = s ^. spawnPosition
        , _bodySize = asteroidMediumSize
        , _bodyAngle = Radians 0
        , _bodyVelocity = asteroidVelocity
        , _bodyAngularVelocity = asteroidAngularVelocity
        }
    , Image asteroidMediumImage)

elapsedTime :: GameSystem Millisecond
elapsedTime = timeDiff <$> getNow <*> use loopGameStart

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
      updateBodies
      spawnEnemies
      maybeShoot
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
  cmap $ \(Player, b@Body {_bodyVelocity = v}) ->
    b {_bodyVelocity = updatePlayerVelocity timeDelta keys v}

draw :: GameSystem ()
draw = do
  renderer <- use loopRenderer
  liftIO (clear renderer)
  atlasCache <- use loopAtlasCache
  starfield <- use loopStarfield
  drawStarfield renderer atlasCache starfield
  cmapM_ $ \(Body pos size angle _ _, Image (ImageIdentifier atlasPath atlasName)) -> do
    foundAtlas <- loadAtlasCached atlasCache atlasPath
    let atlasRect = foundAtlas ^?! atlasFrames . ix atlasName
    copyEx
      renderer
      (foundAtlas ^. atlasTexture)
      (Just (atlasRect ^. to (fromIntegral <$>) . sdlRect))
      (Just (Rectangle (round <$> pos) (fromIntegral <$> size) ^. sdlRect))
      (angle ^. degrees . getDegrees . to realToFrac)
      Nothing
      (V2 False False)
  liftIO (present renderer)

withBackgroundMusic :: IO c -> IO c
withBackgroundMusic x =
  bracket
    (load backgroundMusicPath)
    free
    (\music -> playMusic Forever music >> x)

runLoop :: LoopData -> LoopState a -> IO ()
runLoop initialValue x = void (evalStateT x initialValue)

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
            runLoop initialLoopData (runWith w (initEcs >> mainLoop))
