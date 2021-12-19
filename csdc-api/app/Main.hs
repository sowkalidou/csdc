{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Context (..), readConfig, readSecret, showConfig, activate)

import qualified CSDC.API.Auth as Auth
import qualified CSDC.Action as Action
import qualified CSDC.SQL as SQL

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Middleware.Gzip (gzip, def, gzipFiles, GzipFiles (..))
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serveWithContext, hoistServerWithContext)
import System.Environment (getArgs)

import qualified Network.Wai.Middleware.Cors as Cors

args :: IO (FilePath, Maybe FilePath)
args =
  getArgs >>= \case
    configPath:[] ->
      pure (configPath, Nothing)
    configPath:secretPath:[] ->
      pure (configPath, Just secretPath)
    _ ->
      error "Usage: csdc-server CONFIG_PATH [SECRET_PATH]"

main :: IO ()
main = do
  (configPath, secretPath) <- args
  readConfig configPath >>= \case
    Nothing ->
      error "Could not parse the configuration file."
    Just config ->
      readSecret secretPath >>= \case
        Nothing ->
          error "Could not parse the secrets file."
        Just secret -> do
          putStrLn "Starting the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          context <- activate config secret
          putStrLn "Applying migrations..."
          migrate context
          putStrLn "Server ready."
          mainWith context

mainWith :: Context -> IO ()
mainWith context =
  withStdoutLogger $ \logger -> do
    let port = context_port context
        path = context_path context
        settings = setPort port $ setLogger logger defaultSettings
    authSettings <- Auth.makeSettings
    runSettings settings $ middleware $
      application path authSettings $ context_dao context

middleware :: Middleware
middleware =
  let
    corsOptions = Cors.simpleCorsResourcePolicy
     { Cors.corsRequestHeaders = Cors.simpleHeaders }
    cors = Cors.cors (\_ -> Just corsOptions)
    compress = gzip def { gzipFiles = GzipCompress }
  in
    compress . cors

application :: FilePath -> Auth.Settings -> Action.Context () -> Application
application path settings context = \request response -> do
  let
    api = Proxy @API
    sqlContext = Action.context_sql context
    cfg = Auth.makeContext settings
    server = hoistServerWithContext api Auth.contextProxy (Action.run context) (serveAPI path sqlContext settings)
  serveWithContext api cfg server request response

migrate :: Context -> IO ()
migrate context = do
  let path = context_migration context
  Action.run (context_dao context) $ Action.runSQL $ SQL.migrate path
