{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CSDC.API
  ( API
  , serveAPI
    -- * Utils
  , Auth
  ) where

import CSDC.Auth (getUserToken)
import CSDC.Prelude
import CSDC.User (runUserT)

import qualified CSDC.API.Network as Network

import Servant
import Servant.Server.Internal.Delayed (passToServer)

import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

--------------------------------------------------------------------------------
-- API

type API =
       Auth :> "api" :> Network.API
  :<|> Raw

serveAPI :: MonadNetwork m => FilePath -> ServerT API m
serveAPI path =
         serveNetworkAPI
    :<|> serveDirectoryWith (options path)
  where
    serveNetworkAPI token =
      hoistServer (Proxy @Network.API) (runUserT token) Network.serveAPI

options :: FilePath -> StaticSettings
options path =
  let
    base = defaultWebAppSettings path

    indexRedirect old = \case
      [] -> old [ unsafeToPiece "index.html" ]
      pcs -> old pcs
  in
    base { ssLookupFile = indexRedirect (ssLookupFile base) }

--------------------------------------------------------------------------------
-- Auth

-- | This type is used for representing authentication credentials at the
-- servant API level.
data Auth

instance HasServer api context => HasServer (Auth :> api) context where
  type ServerT (Auth :> api) m = UserToken -> ServerT api m

  route Proxy context subserver =
    route (Proxy @api) context (passToServer subserver getUserToken)

  hoistServerWithContext _ pc nat s =
    hoistServerWithContext (Proxy @api) pc nat . s
