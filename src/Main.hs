{-# LANGUAGE OverloadedStrings #-}

module Main where

import Apecs (cmapM_, global, modify, newEntity, runWith)
import Control.Exception (bracket, bracket_)
import Control.Lens
  ( (&)
  , (.~)
  , (<.)
  , (^.)
  , (^..)
  , (^?!)
  , (^@..)
  , _2
  , asIndex
  , at
  , folded
  , from
  , ifolded
  , ix
  , over
  , set
  , to
  , toListOf
  , view
  )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (_Integer, key, members)
import Data.Foldable (fold, traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.StateVar (($=))
import Data.Text (Text, intercalate)
import Data.Text.IO (putStrLn)
import Data.Text.Lens (unpacked)
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
import SDL.Image (loadTexture)
import SDL.Init (InitFlag(InitAudio, InitVideo), initialize, quit)
import SDL.Input.Keyboard (Keysym(Keysym))
import qualified SDL.Input.Keyboard.Codes as KC
import SDL.Video
  ( Renderer
  , RendererConfig
  , Texture
  , WindowConfig(windowInitialSize, windowResizable)
  , createRenderer
  , createWindow
  , defaultRenderer
  , defaultWindow
  , destroyRenderer
  , destroyWindow
  , rendererLogicalSize
  )
import SDL.Video.Renderer (clear, copyEx, destroyTexture, present)
import SG.Atlas
import SG.Constants
import SG.Math
import SG.TextureCache
import SG.Types
import System.Clock
  ( Clock(Monotonic)
  , TimeSpec
  , diffTimeSpec
  , getTime
  , toNanoSecs
  )
import System.FilePath.Lens (extension)

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

incrTime :: Double -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

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
  incrTime (loopData ^. loopDelta)
  eventResult <- fold <$> mapM eventHandler (eventPayload <$> events)
  case eventResult of
    Finished -> pure ()
    NotFinished -> do
      draw loopData
      afterFrame <- liftIO getMyTime
      let timeDiffSecs =
            fromIntegral (toNanoSecs (afterFrame `diffTimeSpec` beforeFrame)) /
            1000000000.0
      mainLoop (loopData & loopDelta .~ timeDiffSecs)

draw :: LoopData -> System' ()
draw loopData = do
  liftIO (clear (loopData ^. loopRenderer))
  cmapM_ $ \(Body pos size angle velocity angularVelocity, Image (ImageIdentifier atlasPath atlasName)) -> do
    foundAtlas <- loadAtlasCached (loopData ^. loopAtlasCache) atlasPath
    let frames :: [Text]
        frames = foundAtlas ^.. atlasFrames . ifolded . asIndex
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

main :: IO ()
main =
  bracket_ (initialize [InitVideo, InitAudio]) quit $
  bracket (createWindow "hspacegame" windowConfig) destroyWindow $ \window ->
    bracket (createRenderer window (-1) rendererConfig) destroyRenderer $ \renderer ->
      bracket (initTextureCache renderer) destroyTextureCache $ \textureCache -> do
        bracket (initAtlasCache textureCache) destroyAtlasCache $ \atlasCache -> do
          rendererLogicalSize renderer $= Just (fromIntegral <$> gameSize)
          w <- initWorld
          runWith w $ do
            initEcs
            mainLoop (LoopData textureCache atlasCache renderer 0 w)
