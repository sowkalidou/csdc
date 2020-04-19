{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Config (..), readConfig, showConfig)
import CSDC.ORCID (getUserIdentity)
import CSDC.Network.Mock (Store, makeEmptyStore, runMock)

import Control.Concurrent.MVar (MVar)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors as Cors
import Servant (Application, Proxy (..), serve, hoistServer)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [] ->
      error "First argument must be the configuration file path."
    path:_ ->
      readConfig path >>= \case
        Nothing ->
          error "Could not parse the configuration file."
        Just config -> do
          putStrLn "Running the server with the following configuration:\n"
          showConfig config
          putStrLn ""
          mainWith config

mainWith :: Config -> IO ()
mainWith config = do
  store <- makeEmptyStore
  withStdoutLogger $ \logger -> do
    let port = config_port config
        path = config_path config
        settings = setPort port $ setLogger logger defaultSettings
    runSettings settings $ application path store

application :: FilePath -> MVar Store -> Application
application path store =
    printIdentity $
    Cors.cors (\_ -> Just options) $
    serve proxy (hoistServer proxy (runMock store) (serveAPI path))
  where
    printIdentity app req res = do
      print $ getUserIdentity req
      app req res

    options = Cors.simpleCorsResourcePolicy
      { corsRequestHeaders = Cors.simpleHeaders }
    proxy = Proxy @API


