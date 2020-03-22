{-# LANGUAGE TypeApplications #-}

module Main where

import CSDC.API.Network
import CSDC.Network.Mock

import Control.Concurrent.MVar (MVar)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors as Cors
import Servant

main :: IO ()
main = do
  store <- makeEmptyStore
  putStrLn "Running!"
  run 8080 (application store)

proxy :: Proxy API
proxy = Proxy @API

application :: MVar Store -> Application
application store =
    Cors.cors (\_ -> Just options) $
    serve proxy (hoistServer proxy (runMock store) serveAPI)
  where
    options = Cors.simpleCorsResourcePolicy
      { corsRequestHeaders = Cors.simpleHeaders }

