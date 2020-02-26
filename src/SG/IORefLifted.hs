module SG.IORefLifted where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IORef as IORef

readIORef :: MonadIO m => IORef.IORef a -> m a
readIORef = liftIO . IORef.readIORef

writeIORef :: MonadIO m => IORef.IORef a -> a -> m ()
writeIORef m a = liftIO (IORef.writeIORef m a)

newIORef :: MonadIO m => a -> m (IORef.IORef a)
newIORef = liftIO . IORef.newIORef
