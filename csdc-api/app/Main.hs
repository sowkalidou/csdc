{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Context (..), readConfig, readSecret, showConfig, activate)

import qualified CSDC.Auth as Auth
import qualified CSDC.DAO as DAO

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Servant (Application, Proxy (..), serve, hoistServer)
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
          putStrLn "Running the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          context <- activate config secret
          DAO.run (context_dao context) DAO.check
          mainWith context

mainWith :: Context -> IO ()
mainWith context = do
  middleware <- makeMiddleware context
  withStdoutLogger $ \logger -> do
    let port = context_port context
        path = context_path context
        settings = setPort port $ setLogger logger defaultSettings
    runSettings settings $ middleware $ application path $ context_dao context

makeMiddleware :: Context -> IO Middleware
makeMiddleware context = do
  authentication <- Auth.middleware (context_auth context)
  let corsOptions = Cors.simpleCorsResourcePolicy
       { Cors.corsRequestHeaders = Cors.simpleHeaders }
      cors = Cors.cors (\_ -> Just corsOptions)
  pure $ authentication . cors

application :: FilePath -> DAO.Context -> Application
application path context = \request response ->
  let
    proxy = Proxy @API
    server = hoistServer proxy (DAO.run context) (serveAPI path)
  in do
    serve proxy server request response
