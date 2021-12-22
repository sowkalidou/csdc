module CSDC.Daemon
  ( Daemon
  , make
  , launch
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..))
import Data.Void (Void)
import UnliftIO (MonadIO (..), MonadUnliftIO (..))
import UnliftIO.Async (Async)

import qualified UnliftIO
import qualified UnliftIO.Async

-- | A daemon that runs an action in a loop, without ever returning.
newtype Daemon m = Daemon (m Void)

-- | Loops an action every @n@ seconds. Never returns. When the action crashes,
-- errors are catched and the loop is resumed.
loop ::
  MonadUnliftIO m =>
  Int -> String -> m () -> m Void
loop n name act = do
  --  Wait the given quantity of time.
  liftIO $ threadDelay (n * 1000000)

  -- Execute and catch errors.
  UnliftIO.catchAny act $ \err ->
    liftIO $ putStrLn $ "Error in " <> name <> " daemon: " <> displayException err

  -- Repeat.
  loop n name act

-- | Create a @Daemon@ that repeats the same action every @n@ seconds.
make ::
  MonadUnliftIO m =>
  Int -> String -> m () -> Daemon m
make n name act = Daemon (loop n name act)

-- | Create a new thread and launch the @Daemon@ in it. Returns the async that
-- was created.
launch :: MonadUnliftIO m => Daemon m -> m (Async Void)
launch (Daemon act) = UnliftIO.Async.async act
