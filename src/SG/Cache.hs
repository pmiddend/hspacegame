module SG.Cache where

import Control.Lens ((^..), _3, at, folded, over, set, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)

type Constructor a b = a -> IO b

type Destructor b = b -> IO ()

type Cache a b = IORef (Constructor a b, Destructor b, Map a b)

initCache ::
     (Ord a, MonadIO m) => Constructor a b -> Destructor b -> m (Cache a b)
initCache c d = liftIO (newIORef (c, d, mempty))

destroyCache :: MonadIO m => Cache a b -> m ()
destroyCache cache
  -- TODO: Little uneasy with the lenses here
 = do
  (_, destructor, entries) <- liftIO (readIORef cache)
  let resources = entries ^.. folded
  liftIO (traverse_ destructor resources)

loadCached :: (Show a, Ord a, MonadIO m) => Cache a b -> a -> m b
loadCached cache fp = do
  (constructor, _, existingResource) <-
    over _3 (view (at fp)) <$> liftIO (readIORef cache)
  case existingResource of
    Nothing -> do
      liftIO (putStrLn ("loading resource from file " <> show fp))
      resource <- liftIO (constructor fp)
      liftIO (modifyIORef cache (set (_3 . at fp) (Just resource)))
      pure resource
    Just resource -> pure resource
