{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL
  ( -- * Config and Secret
    Config (..),
    parseURL,

    -- * Context
    Context,
    activate,

    -- * Error
    Error (..),

    -- * Action
    Action (..),
    run,
    runAndThrow,
    query,

    -- * Migration
    migrate,
  )
where

import CSDC.Prelude
import Control.Exception (Exception, finally, throwIO, try)
import Control.Monad (forM_)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Connection qualified as Connection
import Hasql.Migration qualified as Migration
import Hasql.Session (QueryError)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.Transaction.Sessions qualified as Transaction
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Config and Secret

-- | The configuration of the PostgreSQL server.
data Config = Config
  { host :: Text,
    port :: Int,
    user :: Text,
    database :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- Like postgresql://csdc:csdc@localhost:5432
parseURL :: String -> Maybe Config
parseURL txt = do
  uri <- parseURI txt
  auth <- uriAuthority uri
  port <- readMaybe $ tail $ uriPort auth
  let (user, pwd) = span (/= ':') $ uriUserInfo auth
      password = init $ tail pwd
  pure
    Config
      { host = Text.pack $ uriRegName auth,
        port = port,
        user = Text.pack $ user,
        database = "csdc", -- hardcoded
        password = Text.pack $ password
      }

--------------------------------------------------------------------------------
-- Context

-- | The context of the PostgreSQL server.
data Context = Context
  { config :: Config
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

-- | Activate the configuration.
-- TODO: Check the validity of the configuration.
activate :: Config -> IO Context
activate config = pure $ Context config

--------------------------------------------------------------------------------
-- Error

-- | The errors that can be emitted from SQL actions.
data Error
  = ErrorConnection ConnectionError
  | ErrorQuery QueryError
  | ErrorMigration Migration.MigrationError
  deriving (Show, Eq)

instance Exception Error

--------------------------------------------------------------------------------
-- Action

-- | An action performing SQL operations.
newtype Action a = Action (ReaderT Connection IO a)
  deriving newtype (Functor, Applicative, Monad)

-- | Run a SQL action and return possible errors.
run :: MonadIO m => Context -> Action a -> m (Either Error a)
run (Context Config {..}) (Action m) = liftIO $ do
  let settings =
        Connection.settings
          (Text.Encoding.encodeUtf8 host)
          (fromIntegral port)
          (Text.Encoding.encodeUtf8 user)
          (Text.Encoding.encodeUtf8 password)
          (Text.Encoding.encodeUtf8 database)

  Connection.acquire settings >>= \case
    Left err ->
      pure $ Left $ ErrorConnection err
    Right conn ->
      try (runReaderT m conn) `finally` (Connection.release conn)

runAndThrow :: MonadIO m => Context -> Action a -> m a
runAndThrow ctx act = do
  result <- run ctx act
  case result of
    Left e -> liftIO $ throwIO e
    Right a -> pure a

-- | Lift a SQL statement into an action.
query :: Statement a b -> a -> Action b
query stm a = Action $ do
  conn <- ask
  let session = Session.statement a stm
  liftIO (Session.run session conn) >>= \case
    Left err ->
      liftIO $ throwIO $ ErrorQuery err
    Right res ->
      pure res

--------------------------------------------------------------------------------
-- Migration

migrate :: FilePath -> Action ()
migrate path = Action $ do
  commands <- liftIO $ Migration.loadMigrationsFromDirectory path
  let transactions =
        fmap Migration.runMigration $
          Migration.MigrationInitialization : commands
      toSession =
        Transaction.transaction Transaction.ReadCommitted Transaction.Write
      sessions = fmap toSession transactions
  conn <- ask
  liftIO $ forM_ sessions $ \session -> do
    res <- Session.run session conn
    case res of
      Left e -> liftIO $ throwIO $ ErrorQuery e
      Right (Just e) -> liftIO $ throwIO $ ErrorMigration e
      Right Nothing -> pure ()
