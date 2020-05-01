{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module CSDC.API
  ( API
  , serveAPI
  ) where

import CSDC.Auth (getUserToken)
import CSDC.Prelude

import qualified CSDC.API.Network as Network

import Servant
import Servant.Server.Internal.Delayed (passToServer)

import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

--------------------------------------------------------------------------------
-- API

type API =
       UserCredentials :> "api" :> Network.API
  :<|> Raw

serveAPI :: MonadNetwork m => FilePath -> ServerT API m
serveAPI path =
       -- XXX: use credentials
       (\_ -> Network.serveAPI)
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

--------------------------------------------------------------------------------
-- UserCredentials

-- | This type is used for representing credentials at the servant API level.
data UserCredentials

instance HasServer api context => HasServer (UserCredentials :> api) context where
  type ServerT (UserCredentials :> api) m = UserToken -> ServerT api m

  route Proxy context subserver =
    route (Proxy @api) context (passToServer subserver getUserToken)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
