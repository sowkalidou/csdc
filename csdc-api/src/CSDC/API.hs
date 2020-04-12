module CSDC.API
  ( API
  , serveAPI
  ) where

import CSDC.Prelude

import qualified CSDC.API.Network as Network

import Servant

import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

--------------------------------------------------------------------------------
-- API

type API =
       "api" :> Network.API
  :<|> Raw

serveAPI :: MonadNetwork m => FilePath -> ServerT API m
serveAPI path =
       Network.serveAPI
  :<|> serveDirectoryWith (options path)

options :: FilePath -> StaticSettings
options path =
  let
    base = defaultWebAppSettings path

    indexRedirect old = \case
      [] -> old [ unsafeToPiece "index.html" ]
      pcs -> old pcs
  in
    base { ssLookupFile = indexRedirect (ssLookupFile base) }
