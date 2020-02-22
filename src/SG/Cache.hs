module SG.Cache where

import Control.Lens ((^..), _3, at, folded, over, set, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import System.FilePath (FilePath)

type Constructor b = FilePath -> IO b

type Destructor b = b -> IO ()

type Cache b = IORef (Constructor b, Destructor b, Map FilePath b)

initCache :: MonadIO m => Constructor b -> Destructor b -> m (Cache b)
initCache c d = liftIO (newIORef (c, d, mempty))

destroyCache :: MonadIO m => Cache b -> m ()
destroyCache cache
  -- TODO: Little uneasy with the lenses here
 = do
  (_, destructor, entries) <- liftIO (readIORef cache)
  let resources = entries ^.. folded
  liftIO (traverse_ destructor resources)

loadCached :: MonadIO m => Cache b -> FilePath -> m b
loadCached cache fp = do
  (constructor, _, existingResource) <-
    over _3 (view (at fp)) <$> liftIO (readIORef cache)
  case existingResource of
    Nothing -> do
      liftIO (putStrLn ("loading resource from file " <> fp))
      resource <- liftIO (constructor fp)
      liftIO (modifyIORef cache (set (_3 . at fp) (Just resource)))
      pure resource
    Just resource -> pure resource
