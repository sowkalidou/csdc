module CSDC.FileServer
  ( serveSQLFileServer
  ) where

import CSDC.Data.File (FileDB (..))

import qualified CSDC.SQL as SQL
import qualified CSDC.SQL.Files as SQL.Files

import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Network.Wai.Application.Static
import Servant.Server.StaticFiles
import Servant (ServerT, Raw)
import System.FilePath (takeFileName)

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Network.Wai as Wai
import qualified WaiAppStatic.Types as Static
import qualified Data.Text as Text

lookupFile :: SQL.Context -> Static.Pieces -> IO Static.LookupResult
lookupFile ctx pieces = do
  let filepath = Text.intercalate "/" $ fmap Static.fromPiece pieces
  result <- SQL.run ctx $ SQL.query SQL.Files.select filepath
  case result of
    Left e -> throwIO e
    Right Nothing -> pure Static.LRNotFound
    Right (Just file) -> pure $ Static.LRFile Static.File
      { Static.fileGetSize =
          fromIntegral $ fileDB_size file
      , Static.fileToResponse = \status headers ->
          Wai.responseLBS status headers $
          ByteString.Lazy.fromStrict $ fileDB_contents file
      , Static.fileName =
          fromMaybe (error "fromPiece: name has /") $
          Static.toPiece $
          Text.pack $ takeFileName $
          Text.unpack $ fileDB_filepath file
      , Static.fileGetHash =
          pure $ Just $ fileDB_hash file
      , Static.fileGetModified =
          Just $ fileDB_modifiedAt file
      }

staticSettings :: SQL.Context ->StaticSettings
staticSettings ctx = (defaultWebAppSettings "XXX")
  { ssLookupFile = lookupFile ctx
  , ssMaxAge = Static.NoMaxAge
  }

serveSQLFileServer :: SQL.Context -> ServerT Raw m
serveSQLFileServer ctx = serveDirectoryWith (staticSettings ctx)
