{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket, bracket_)
import Data.StateVar (($=))
import Linear.V2 (V2(V2))
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
  , Window
  , WindowConfig(windowInitialSize, windowResizable)
  , createRenderer
  , createWindow
  , defaultRenderer
  , defaultWindow
  , destroyRenderer
  , destroyWindow
  , rendererLogicalSize
  )

gameSize :: V2 Int
gameSize = V2 1024 768

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

eventHandler :: EventPayload -> IO FinishState
eventHandler QuitEvent = pure Finished
eventHandler (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KC.KeycodeEscape _))) =
  pure Finished
eventHandler _ = mempty

mainLoop :: Renderer -> IO ()
mainLoop renderer = do
  events <- pollEvents
  eventResult <- foldMap eventHandler (eventPayload <$> events)
  case eventResult of
    NotFinished -> mainLoop renderer
    Finished -> pure ()

main :: IO ()
main =
  bracket_ (initialize [InitVideo, InitAudio]) quit $
  bracket (createWindow "hspacegame" windowConfig) destroyWindow $ \window ->
    bracket (createRenderer window (-1) rendererConfig) destroyRenderer $ \renderer -> do
      rendererLogicalSize renderer $= Just (fromIntegral <$> gameSize)
      mainLoop renderer
