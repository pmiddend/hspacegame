{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module SG.LoopData where

import Apecs (Entity, SystemT, destroy)
import Control.Exception (bracket)
import Control.Exception.Lifted (catch)
import Control.Lens (makeLenses, use)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Proxy
import Linear.V2 (V2)
import SDL.Exception (SDLException)
import SDL.Mixer (Chunk, pattern Forever, free, load, play, playMusic)
import SDL.Video (Renderer)
import SG.Atlas
import SG.ChunkCache
import SG.Constants
import SG.Font
import SG.Starfield
import SG.TextureCache
import SG.Time
import SG.Types
import System.Random (StdGen)

type PlayerKeys = V2 Int

data LoopData =
  LoopData
    { _loopTextureCache :: TextureCache
    , _loopAtlasCache :: AtlasCache
    , _loopChunkCache :: ChunkCache
    , _loopRenderer :: Renderer
    , _loopTextCache :: TextCache
    , _loopFontCache :: FontCache
    , _loopRng :: StdGen
    , _loopDelta :: Double
    , _loopWorld :: World
    , _loopStarfield :: Starfield
    , _loopPlayerKeys :: PlayerKeys
    , _loopLevel :: Level
    , _loopGameStart :: TimePoint
    , _loopScore :: Score
    , _loopCurrentEnergy :: Energy
    , _loopMaxEnergy :: Energy
    , _loopGameState :: GameState
    , _loopPlayer :: Entity
    , _loopHint :: Maybe Hint
    }

makeLenses ''LoopData

--type GameSystem a = StateT LoopData (SystemT World IO) a
type LoopState = StateT LoopData IO

type GameSystem = SystemT World LoopState

instance MonadState s m => MonadState s (SystemT w m) where
  get = lift get
  put s = lift (put s)

playCatchIO :: MonadIO m => Chunk -> m ()
playCatchIO c =
  liftIO
    (play c `catch` \e ->
       putStrLn ("play sound failed" <> show (e :: SDLException)))

playChunk :: FilePath -> GameSystem ()
playChunk fp = do
  chunkCache <- use loopChunkCache
  c <- loadChunk chunkCache fp
  playCatchIO c

elapsedTime :: GameSystem Duration
elapsedTime = timeDiff <$> getNow <*> use loopGameStart

destroyEntity ety = destroy ety (Proxy @AllComponents)

runLoop :: LoopData -> LoopState a -> IO ()
runLoop initialValue x = void (evalStateT x initialValue)

withBackgroundMusic :: IO c -> IO c
withBackgroundMusic x =
  bracket
    (load backgroundMusicPath)
    free
    (\music -> playMusic Forever music >> x)
