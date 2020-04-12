{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API (API, serveAPI)
import CSDC.Config (Config (..), readConfig, showConfig)
import CSDC.Network.Mock (Store, makeEmptyStore, runMock)

import Control.Concurrent.MVar (MVar)
import Network.Wai.Handler.Warp (run)
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
          store <- makeEmptyStore
          run (config_port config) (application (config_path config) store)

proxy :: Proxy API
proxy = Proxy @API

application :: FilePath -> MVar Store -> Application
application path store =
    Cors.cors (\_ -> Just options) $
    serve proxy (hoistServer proxy (runMock store) (serveAPI path))
  where
    options = Cors.simpleCorsResourcePolicy
      { corsRequestHeaders = Cors.simpleHeaders }

