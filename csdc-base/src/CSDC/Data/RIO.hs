module CSDC.Data.RIO
  ( RIO
  , runRIO
  ) where

import Control.Concurrent.MVar (MVar, readMVar, modifyMVar)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (MonadState (..))

newtype RIO s m a = RIO (ReaderT (MVar s) m a)
  deriving newtype (Functor, Applicative, Monad)

runRIO :: MonadIO m => MVar s -> RIO s m a -> m a
runRIO var (RIO m) = runReaderT m var

instance MonadIO m => MonadState s (RIO s m) where
  get = RIO $ do
    var <- ask
    liftIO $ readMVar var

  state f = RIO $ do
    var <- ask
    let f' s = do
          let (s',a) = f s
          pure (a,s')
    liftIO $ modifyMVar var f'
