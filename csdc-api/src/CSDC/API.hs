{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CSDC.API
  ( API,
    serveAPI,
  )
where

import CSDC.API.Auth qualified as Auth
import CSDC.Action
import CSDC.FileServer (serveSQLFileServer)
import CSDC.SQL qualified as SQL
import Servant hiding (Server)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

--------------------------------------------------------------------------------
-- API

type API =
  Auth.API
    :<|> "files" :> Raw
    :<|> Raw

serveAPI :: FilePath -> SQL.Context -> Auth.Settings -> Server API
serveAPI path ctx settings =
  Auth.serveAPI settings
    :<|> serveSQLFileServer ctx
    :<|> serveDirectoryWith (options path)

options :: FilePath -> StaticSettings
options path =
  let base = defaultWebAppSettings path

      indexRedirect old = \case
        [] -> old [unsafeToPiece "index.html"]
        pcs -> old pcs
   in base {ssLookupFile = indexRedirect (ssLookupFile base)}
