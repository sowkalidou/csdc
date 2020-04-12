module CSDC.API
  ( API
  , serveAPI
  ) where

import CSDC.Prelude

import qualified CSDC.API.Network as Network

import Servant
import Servant.Server.StaticFiles (serveDirectoryWebApp)

--------------------------------------------------------------------------------
-- API

type API = Network.API :<|> Raw

serveAPI :: MonadNetwork m => FilePath -> ServerT API m
serveAPI path =
       Network.serveAPI
  :<|> serveDirectoryWebApp path
