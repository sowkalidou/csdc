{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.IPFS
  ( -- * General
    Config (..),
    Context (..),
    activate,
    Action (..),
    run,
    add,
    addJSON,

    -- * Reexport
    CID (..),
  )
where

import CSDC.Prelude
import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Network.IPFS (MonadLocalIPFS (..))
import Network.IPFS.Add (addFile)
import Network.IPFS.CID.Types (CID (..))
import Network.IPFS.Name.Types (Name (..))
import Network.IPFS.Process.Error (Error (..))
import System.Process.Typed

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { path :: FilePath
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

data Context = Context
  { path :: FilePath
  }

activate :: Config -> IO Context
activate Config {..} = do
  pure
    Context
      { path = path
      }

--------------------------------------------------------------------------------
-- Action

newtype Action a = Action (ReaderT Context IO a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader Context, MonadIO)

run :: MonadIO m => Context -> Action a -> m a
run context (Action action) = liftIO $ runReaderT action context

instance MonadLocalIPFS Action where
  runLocal opts arg = do
    Context path <- ask

    let secs = 5
        opts' = ("--timeout=" <> show secs <> "s") : opts
        process =
          setStdin (byteStringInput arg) $
            proc path opts'

    readProcess process >>= \case
      (ExitSuccess, contents, _) ->
        return $ Right contents
      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr ->
            return . Left $ Timeout secs
        | otherwise ->
            return . Left $ UnknownErr stdErr

add :: FilePath -> ByteString -> Action CID
add path bs =
  addFile bs (Name path) >>= \case
    Left e -> liftIO $ throwIO e
    Right (_, a) -> pure a

addJSON :: ToJSON a => FilePath -> a -> Action CID
addJSON path a = add path (encode a)
