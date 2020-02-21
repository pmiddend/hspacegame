{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.Game where

import Apecs (cmapM_, global, modify, newEntity, runWith)
import Control.Exception (bracket, bracket_)
import Control.Lens ((%~), (&), (.~), (^.), (^?!), ix, makeLenses, to)
import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Foldable (fold)
import Data.StateVar (($=))
import Linear.V2 (V2(V2))
import Linear.Vector ((^/))
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

eventHandler :: EventPayload -> System' FinishState
eventHandler QuitEvent = pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
eventHandler _ = pure mempty

initEcs :: System' ()
initEcs = do
  playerEty <-
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

mainLoop :: LoopData -> System' ()
mainLoop loopData = do
  beforeFrame <- liftIO getMyTime
  events <- liftIO pollEvents
  eventResult <- fold <$> mapM eventHandler (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw loopData
      afterFrame <- liftIO getMyTime
      let timeDiffSecs =
            fromIntegral (toNanoSecs (afterFrame `diffTimeSpec` beforeFrame)) /
            1000000000.0
      mainLoop
        (loopData & loopDelta .~ timeDiffSecs & loopStarfield %~
         stepStarfield (loopData ^. loopDelta))

draw :: LoopData -> System' ()
draw loopData = do
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
              mainLoop
                (LoopData
                   textureCache
                   atlasCache
                   renderer
                   0
                   w
                   (initStarfield (mkStdGen 0))
                   initialPlayerKeys)
