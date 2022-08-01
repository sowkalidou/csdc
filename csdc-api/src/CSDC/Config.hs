{-# LANGUAGE StrictData #-}

module CSDC.Config
  ( -- * Config
    Config (..)
  , readConfig
  , showConfig
    -- * Context
  , Context (..)
  , activate
  ) where

import CSDC.Prelude

import qualified CSDC.Action as Action
import qualified CSDC.IPFS as IPFS
import qualified CSDC.Mail as Mail
import qualified CSDC.SQL as SQL

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as ByteString

--------------------------------------------------------------------------------
-- SQL Config

data SQLConfig = SQLConfigFile SQL.Config | SQLConfigEnv String
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON SQLConfig

activateSQL :: SQLConfig -> IO SQL.Context
activateSQL (SQLConfigFile config) = SQL.activate config
activateSQL (SQLConfigEnv var) =
  env var >>= \case
    Nothing ->
      error $ "Could not find variable for SQL configuration named $" <> var
    Just str ->
      case SQL.parseURL (Text.unpack str) of
        Nothing ->
          error $ "Could not parse SQL configuration from $" <> var
        Just config ->
          SQL.activate config

--------------------------------------------------------------------------------
-- Mail Config

data MailConfig =
  MailConfigFile Mail.Config |
  MailConfigEnv String |
  MailConfigDisplay
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON MailConfig

activateMail :: MailConfig -> IO (Maybe Mail.Context)
activateMail (MailConfigFile config) = Just <$> Mail.activate config
activateMail (MailConfigEnv prefix) = do
  mHostName <- lookupEnv (prefix <> "_SMTP_SERVER")
  mPortNumber <- fmap read <$> lookupEnv (prefix <> "_SMTP_PORT")
  mUserName <- lookupEnv (prefix <> "_SMTP_LOGIN")
  mPassword <- lookupEnv (prefix <> "_SMTP_PASSWORD")
  case Mail.Config <$> mHostName <*> mPortNumber <*> mUserName <*> mPassword of
    Nothing -> error "Could not find environment variables"
    Just config -> do
      print config
      Just <$> Mail.activate config
activateMail MailConfigDisplay = pure Nothing

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_port :: Int
  , config_path :: FilePath
  , config_sql :: SQLConfig
  , config_mail :: MailConfig
  , config_ipfs :: IPFS.Config
  , config_migration :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

readConfig :: MonadIO m => FilePath -> m (Either String Config)
readConfig path = liftIO $ do
  mconfig <- eitherDecodeFileStrict path
  mport <- lookupEnv "PORT"
  case readMaybe =<< mport of
    Nothing ->
      pure mconfig
    Just port ->
      case mconfig of
        Left e ->
          pure $ Left e
        Right config ->
          pure $ Right config { config_port = port }

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let
    str = ByteString.unpack $ encodePretty config
  in
    liftIO $ putStrLn str

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { context_port :: Int
  , context_path :: FilePath
  , context_dao :: Action.Context ()
  , context_migration :: FilePath
  } deriving (Generic)

activate :: Config -> IO Context
activate config = do
  sql <- activateSQL (config_sql config)
  mail <- activateMail (config_mail config)
  ipfs <- IPFS.activate (config_ipfs config)
  pure Context
    { context_port = config_port config
    , context_path = config_path config
    , context_dao = Action.Context
        { Action.context_sql = sql
        , Action.context_mail = mail
        , Action.context_ipfs = ipfs
        , Action.context_user = ()
        }
    , context_migration = config_migration config
    }

--------------------------------------------------------------------------------
-- Helper

env :: String -> IO (Maybe Text)
env var = fmap Text.pack <$> lookupEnv var
