module SG.ChunkCache where

import Control.Monad.IO.Class (MonadIO)
import SDL.Mixer (Chunk, free, load)
import SG.Cache

type ChunkCache = Cache FilePath Chunk

initChunkCache :: MonadIO m => m ChunkCache
initChunkCache = initCache load free

destroyChunkCache :: MonadIO m => ChunkCache -> m ()
destroyChunkCache = destroyCache

loadChunk :: MonadIO m => ChunkCache -> FilePath -> m Chunk
loadChunk = loadCached
