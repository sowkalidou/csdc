{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL
  ( -- * Config and Secret
    Config (..)
  , parseURL
    -- * Context
  , Context
  , activate
    -- * Error
  , Error (..)
    -- * Action
  , Action (..)
  , run
  , runAndThrow
  , query
    -- * Migration
  , migrate
  ) where

import CSDC.Prelude

import Control.Exception (Exception, finally, throwIO, try)
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Session (QueryError)
import Hasql.Statement (Statement)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Text.Read (readMaybe)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Migration as Migration
import qualified Hasql.Transaction.Sessions as Transaction

--------------------------------------------------------------------------------
-- Config and Secret

-- | The configuration of the PostgreSQL server.
data Config = Config
  { config_host :: Text
  , config_port :: Int
  , config_user :: Text
  , config_database :: Text
  , config_password :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

-- Like postgresql://csdc:csdc@localhost:5432
parseURL :: String -> Maybe Config
parseURL txt = do
  uri <- parseURI txt
  auth <- uriAuthority uri
  port <- readMaybe $ tail $ uriPort auth
  let (user, pwd) = span (/= ':') $ uriUserInfo auth
      password = init $ tail pwd
  pure Config
    { config_host = Text.pack $ uriRegName auth
    , config_port = port
    , config_user = Text.pack $ user
    , config_database = "csdc" -- hardcoded
    , config_password = Text.pack $ password
    }

--------------------------------------------------------------------------------
-- Context

-- | The context of the PostgreSQL server.
newtype Context = Context
  { context_config :: Config
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

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
  deriving (Functor, Applicative, Monad)

-- | Run a SQL action and return possible errors.
run :: MonadIO m => Context -> Action a -> m (Either Error a)
run (Context Config {..}) (Action m) = liftIO $ do
  let settings =
        Connection.settings
          (Text.Encoding.encodeUtf8 config_host)
          (fromIntegral config_port)
          (Text.Encoding.encodeUtf8 config_user)
          (Text.Encoding.encodeUtf8 config_password)
          (Text.Encoding.encodeUtf8 config_database)

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
  let
    transactions =
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
