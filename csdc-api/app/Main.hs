{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Context (..), readConfig, showConfig, activate)

import qualified CSDC.API.Auth as Auth
import qualified CSDC.Action as Action
import qualified CSDC.Daemon as Daemon
import qualified CSDC.Daemon.Mail as Daemon.Mail
import qualified CSDC.SQL as SQL

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Middleware.Gzip (gzip, def, gzipFiles, GzipFiles (..))
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serveWithContext, hoistServerWithContext)
import System.Environment (getArgs)
import System.IO

import qualified Network.Wai.Middleware.Cors as Cors

args :: IO FilePath
args =
  getArgs >>= \case
    configPath:[] ->
      pure configPath
    _ ->
      error "Usage: csdc-server CONFIG_PATH [SECRET_PATH]"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  configPath <- args
  readConfig configPath >>= \case
    Left e ->
      error $ "Could not parse the configuration file: " <> e
    Right config -> do
      putStrLn "Starting the server with the following configuration:\n"
      showConfig config
      putStrLn ""
      context <- activate config
      putStrLn "Applying migrations..."
      migrate context
      mainWith context

mainWith :: Context -> IO ()
mainWith Context {..} = do
  putStrLn "Starting mail daemon..."
  _ <- Action.run_ context_dao (Daemon.launch Daemon.Mail.daemon)
  putStrLn "Server ready."
  withStdoutLogger $ \logger -> do
    let settings = setPort context_port $ setLogger logger defaultSettings
    authSettings <- Auth.makeSettings
    runSettings settings $ middleware $
      application context_path authSettings context_dao

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
  Action.run_ (context_dao context) $ Action.runSQL $ SQL.migrate path
