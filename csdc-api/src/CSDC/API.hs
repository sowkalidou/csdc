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
import CSDC.DAO
import CSDC.FileServer (serveSQLFileServer)
import CSDC.Prelude

import qualified CSDC.API.DAO as DAO
import qualified CSDC.SQL as SQL

import Servant
import Servant.Server.Internal.Delayed (passToServer)

import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

--------------------------------------------------------------------------------
-- API

type API =
       Auth :> "api" :> DAO.API
  :<|> "files" :> Raw
  :<|> Raw

serveAPI :: FilePath -> SQL.Context -> ServerT API (Action ())
serveAPI path ctx =
         serveDAOAPI
    :<|> serveSQLFileServer ctx
    :<|> serveDirectoryWith (options path)
  where
    serveDAOAPI token =
      hoistServer (Proxy @DAO.API) (withToken token) DAO.serveAPI

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
