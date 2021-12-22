{-# LANGUAGE StrictData #-}

module CSDC.Config
  ( -- * Config
    Config (..)
  , readConfig
  , showConfig
    -- * Secret
  , Secret (..)
  , readSecret
    -- * Context
  , Context (..)
  , activate
  ) where

import CSDC.Prelude

import qualified CSDC.Action as Action
import qualified CSDC.SQL as SQL

import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as ByteString

--------------------------------------------------------------------------------
-- SQL Config

data SQLConfig = SQLConfig SQL.Config | SQLConfigEnv
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SQLConfig

data SQLSecret = SQLSecret SQL.Secret | SQLSecretEnv SQL.Config SQL.Secret
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SQLSecret

getSQLSecret :: IO (Maybe SQLSecret)
getSQLSecret =
  env "DATABASE_URL" >>= \case
    Nothing -> pure Nothing
    Just str -> pure $ do
      (config, secret) <- SQL.parseURL $ Text.unpack str
      pure $ SQLSecretEnv config secret

toSQLContext :: SQLConfig -> SQLSecret -> IO SQL.Context
toSQLContext (SQLConfig config) (SQLSecret secret) =
  SQL.activate config secret
toSQLContext SQLConfigEnv (SQLSecretEnv config secret) =
  SQL.activate config secret
toSQLContext _ _ =
  error "SQL badly configured."

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_port :: Int
  , config_path :: FilePath
  , config_sql :: SQLConfig
  , config_migration :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

readConfig :: MonadIO m => FilePath -> m (Maybe Config)
readConfig path = liftIO $ do
  mconfig <- decodeFileStrict path
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just port ->
      case mconfig of
        Nothing ->
          pure Nothing
        Just config ->
          pure $ Just config { config_port = port }

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let
    str = ByteString.unpack $ encodePretty config
  in
    liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Secret

data Secret = Secret
  { secret_sql :: SQLSecret
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Secret

-- | Read secrets either from a file, or from environment variables.
readSecret :: MonadIO m => Maybe FilePath -> m (Maybe Secret)
readSecret (Just path) = liftIO $ decodeFileStrict path
readSecret Nothing = liftIO $ do
  mSql <- getSQLSecret
  pure $ Secret <$> mSql

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { context_port :: Int
  , context_path :: FilePath
  , context_dao :: Action.Context ()
  , context_migration :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

activate :: Config -> Secret -> IO Context
activate config secret = do
  sql <- toSQLContext (config_sql config) (secret_sql secret)
  pure Context
    { context_port = config_port config
    , context_path = config_path config
    , context_dao = Action.Context
        { Action.context_sql = sql
        , Action.context_mail = undefined
        , Action.context_user = ()
        }
    , context_migration = config_migration config
    }

--------------------------------------------------------------------------------
-- Helper

env :: String -> IO (Maybe Text)
env var = fmap Text.pack <$> lookupEnv var
