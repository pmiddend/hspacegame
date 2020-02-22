{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Game where

import Apecs (cmap, cmapM, cmapM_, destroy, newEntity, runWith)
import Control.Exception (bracket, bracket_)
import Control.Exception.Lifted (catch)
import Control.Lens
  ( (%~)
  , (&)
  , (+~)
  , (.~)
  , (?~)
  , (^.)
  , (^?!)
  , ix
  , makeLenses
  , to
  , view
  )
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default.Class (def)
import Data.Foldable (fold, for_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Monoid (All(All), getAll)
import Data.Proxy
import Data.StateVar (($=))
import Data.Time.Units (Microsecond, toMicroseconds)
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((*^), (^/))
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
    }

makeLenses ''LoopData

type LoopRef = IORef LoopData

readLoopData :: MonadIO m => LoopRef -> m LoopData
readLoopData r = liftIO (readIORef r)

modifyLoopData :: MonadIO m => LoopRef -> Endo LoopData -> m ()
modifyLoopData r f = liftIO (modifyIORef r f)

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

eventHandler :: LoopRef -> EventPayload -> System' FinishState
eventHandler _ QuitEvent = pure Finished
eventHandler _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ spaceStatus _ (Keysym _ KC.KeycodeSpace _))) =
  continue $
  modifyLoopData
    loopRef
    (\ld ->
       ld & loopSpacePressed .~ spaceStatus & loopLastShot %~
       resetIf (spaceStatus == Pressed))
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ kc _))) =
  continue $
  for_
    (keyCodeToPlayerDirection keyState kc)
    (modifyLoopData loopRef . (loopPlayerKeys +~))
eventHandler _ _ = continue (pure ())

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

updateBodies :: LoopRef -> System' ()
updateBodies loopRef = do
  LoopData {_loopDelta = timeDelta} <- readLoopData loopRef
  cmapM $ \(b@Body {}, ety) -> do
    unless (inBounds b) (destroy ety (Proxy @AllComponents))
    pure (b & bodyPosition +~ timeDelta *^ (b ^. bodyVelocity))

initEcs :: System' ()
initEcs = do
  _ <-
    newEntity
      ( Player
      , Body
          { _bodyPosition =
              ((fromIntegral <$> gameSize) ^/ 2.0) -
              (fromIntegral <$> playerSize) ^/
              2.0
          , _bodySize = playerSize
          , _bodyAngle = Radians 0
          , _bodyVelocity = V2 0 0
          , _bodyAngularVelocity = Radians 0
          }
      , Image playerImage)
  return ()

playCatchIO :: MonadIO m => Chunk -> m ()
playCatchIO c =
  liftIO
    (play c `catch` \e ->
       putStrLn ("play sound failed" <> show (e :: SDLException)))

playChunk :: MonadIO m => ChunkCache -> FilePath -> m ()
playChunk cache fp = do
  c <- loadChunk cache fp
  playCatchIO c

maybeShoot :: LoopRef -> System' ()
maybeShoot loopRef = do
  LoopData { _loopSpacePressed = spacePressed
           , _loopLastShot = lastShot
           , _loopChunkCache = chunkCache
           } <- readLoopData loopRef
  now <- getNow
  if spacePressed == Pressed &&
     getAll
       (foldMap (All . (> playerShootingFrequency) . (now `timeDiff`)) lastShot)
    then cmapM_ $ \(Player, Body { _bodyPosition = playerPos
                                 , _bodySize = playerSize'
                                 }) -> do
           void $
             newEntity
               ( Bullet
               , Body
                   { _bodyPosition =
                       V2
                         (playerPos ^. _x + fromIntegral (playerSize' ^. _x) /
                          2.0)
                         (playerPos ^. _y)
                   , _bodySize = laserSize
                   , _bodyAngle = Radians 0
                   , _bodyVelocity = laserSpeed
                   , _bodyAngularVelocity = Radians 0
                   }
               , Image laserImage)
           modifyLoopData loopRef (loopLastShot ?~ now)
           playChunk chunkCache pewPath
    else pure ()

mainLoop :: LoopRef -> System' ()
mainLoop loopRef = do
  beforeFrame <- getNow
  events <- liftIO pollEvents
  eventResult <- fold <$> mapM (eventHandler loopRef) (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw loopRef
      updatePlayer loopRef
      updateBodies loopRef
      maybeShoot loopRef
      afterFrame <- getNow
      let timeDiffSecs =
            fromIntegral
              (toMicroseconds (afterFrame `timeDiff` beforeFrame :: Microsecond)) /
            1000000.0
      modifyLoopData
        loopRef
        (\loopData ->
           loopData & loopDelta .~ timeDiffSecs & loopStarfield %~
           stepStarfield (loopData ^. loopDelta))
      mainLoop loopRef

updatePlayer :: LoopRef -> System' ()
updatePlayer loopRef = do
  LoopData {_loopDelta = timeDelta, _loopPlayerKeys = keys} <-
    readLoopData loopRef
  cmap $ \(Player, b@Body {_bodyVelocity = v}) ->
    b {_bodyVelocity = updatePlayerVelocity timeDelta keys v}

draw :: LoopRef -> System' ()
draw loopRef = do
  loopData <- readLoopData loopRef
  liftIO (clear (loopData ^. loopRenderer))
  drawStarfield
    (loopData ^. loopRenderer)
    (loopData ^. loopAtlasCache)
    (loopData ^. loopStarfield)
  cmapM_ $ \(Body pos size angle _ _, Image (ImageIdentifier atlasPath atlasName)) -> do
    foundAtlas <- loadAtlasCached (loopData ^. loopAtlasCache) atlasPath
    let atlasRect = foundAtlas ^?! atlasFrames . ix atlasName
    copyEx
      (loopData ^. loopRenderer)
      (foundAtlas ^. atlasTexture)
      (Just (atlasRect ^. to (fromIntegral <$>) . sdlRect))
      (Just (Rectangle (round <$> pos) (fromIntegral <$> size) ^. sdlRect))
      (angle ^. degrees . getDegrees . to realToFrac)
      Nothing
      (V2 False False)
  liftIO (present (loopData ^. loopRenderer))

withBackgroundMusic :: IO c -> IO c
withBackgroundMusic x =
  bracket
    (load backgroundMusicPath)
    free
    (\music -> playMusic Forever music >> x)

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
            runWith w $ do
              initEcs
              loopRef <-
                liftIO $
                newIORef
                  (LoopData
                     textureCache
                     atlasCache
                     chunkCache
                     renderer
                     0
                     w
                     (initStarfield (mkStdGen 0))
                     initialPlayerDirection
                     Released
                     Nothing)
              mainLoop loopRef
