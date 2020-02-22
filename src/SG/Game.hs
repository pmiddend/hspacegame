{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Game where

import Debug.Trace(traceShowId)
import Apecs (cmapM_, newEntity, runWith, cmap)
import Control.Exception (bracket, bracket_)
import Control.Lens ((%~), (&), (.~), (^.), (^?!), ix, makeLenses, to, Lens', (+~), view)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default.Class (def)
import Data.Foldable (fold)
import Data.StateVar (($=))
import Linear.V2 (V2(V2), _x, _y)
import Linear.Vector ((^/), (*^))
import Prelude hiding (lookup, putStrLn)
import SDL.Event
  ( EventPayload(KeyboardEvent, QuitEvent)
  , InputMotion(Pressed)
  , KeyboardEventData(KeyboardEventData)
  , eventPayload
  , pollEvents
  )
import SDL.Init (InitFlag(InitAudio, InitVideo), initialize, quit)
import SDL.Input.Keyboard (Keysym(Keysym))
import qualified SDL.Input.Keyboard.Codes as KC
import SDL.Mixer (pattern Forever, free, load, playMusic, withAudio)
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
import SG.Constants
import SG.Math
import SG.Starfield
import SG.TextureCache
import SG.Types
import System.Clock
  ( Clock(Monotonic)
  , TimeSpec
  , diffTimeSpec
  , getTime
  , toNanoSecs
  )
import System.Random (mkStdGen)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef)

type PlayerKeys = V2 Int

data LoopData =
  LoopData
    { _loopTextureCache :: TextureCache
    , _loopAtlasCache :: AtlasCache
    , _loopRenderer :: Renderer
    , _loopDelta :: Double
    , _loopWorld :: World
    , _loopStarfield :: Starfield
    , _loopPlayerKeys :: PlayerKeys
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

eventHandler :: LoopRef -> EventPayload -> System' FinishState
eventHandler _ QuitEvent = pure Finished
eventHandler _ (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
-- TODO repetitive
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ KC.KeycodeA _))) = do
  modifyLoopData loopRef (loopPlayerKeys . _x +~ (if keyState == Pressed then -1 else 1))
  pure mempty
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ KC.KeycodeD _))) = do
  modifyLoopData loopRef (loopPlayerKeys . _x +~ (if keyState == Pressed then 1 else -1))
  pure mempty
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ KC.KeycodeW _))) = do
  modifyLoopData loopRef (loopPlayerKeys . _y +~ (if keyState == Pressed then -1 else 1))
  pure mempty
eventHandler loopRef (KeyboardEvent (KeyboardEventData _ keyState False (Keysym _ KC.KeycodeS _))) = do
  modifyLoopData loopRef (loopPlayerKeys . _y +~ (if keyState == Pressed then 1 else -1))
  pure mempty
eventHandler _ _ = pure mempty

updatePlayerVelocity :: Double -> PlayerKeys -> Endo (V2 Double)
updatePlayerVelocity timeDelta pk v =
  let updatePlayerVelocity' :: (forall a . V2 a -> a) -> Double
      updatePlayerVelocity' getter = 
        let cv = getter v
            velocitySignum = getter pk
            inverseSignum = (-1) * signum cv
            playerFrictionPart = abs cv / getter playerMaxVelocity
        in if velocitySignum /= 0
        then fromIntegral velocitySignum * getter playerMaxVelocity
        else if abs cv < 0.000001
             then 0
             else cv + inverseSignum * timeDelta * playerFrictionPart * getter playerFriction
  in V2 (updatePlayerVelocity' (view _x)) (updatePlayerVelocity' (view _y))

updateBodies :: LoopRef -> System' ()
updateBodies loopRef = do
  LoopData { _loopDelta = timeDelta } <- readLoopData loopRef
  cmap $ \b@Body { } -> b & bodyPosition +~ timeDelta *^ (b ^. bodyVelocity)

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

getMyTime :: IO TimeSpec
getMyTime = getTime Monotonic

mainLoop :: LoopRef -> System' ()
mainLoop loopRef = do
  beforeFrame <- liftIO getMyTime
  events <- liftIO pollEvents
  eventResult <- fold <$> mapM (eventHandler loopRef) (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw loopRef
      updatePlayer loopRef
      updateBodies loopRef
      afterFrame <- liftIO getMyTime
      let timeDiffSecs =
            fromIntegral (toNanoSecs (afterFrame `diffTimeSpec` beforeFrame)) /
            1000000000.0
      modifyLoopData loopRef (\loopData -> loopData & loopDelta .~ timeDiffSecs & loopStarfield %~
         stepStarfield (loopData ^. loopDelta))
      mainLoop loopRef

updatePlayer :: LoopRef -> System' ()
updatePlayer loopRef = do
  LoopData { _loopDelta = timeDelta, _loopPlayerKeys = keys } <- readLoopData loopRef
  cmap $ \(Player, b@Body { _bodyVelocity = v }) -> b { _bodyVelocity = updatePlayerVelocity timeDelta keys v }

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
          withAudio def 1024 $ withBackgroundMusic $ do
            rendererLogicalSize renderer $= Just (fromIntegral <$> gameSize)
            w <- initWorld
            runWith w $ do
              initEcs
              loopRef <- liftIO $ newIORef (LoopData
                   textureCache
                   atlasCache
                   renderer
                   0
                   w
                   (initStarfield (mkStdGen 0))
                   initialPlayerDirection)
              mainLoop loopRef
                
