module CSDC.FileServer
  ( serveSQLFileServer
  ) where

import CSDC.Data.File (FileDB (..))

import qualified CSDC.SQL as SQL
import qualified CSDC.SQL.Files as SQL.Files

import Data.Text (Text)
import Hasql.Statement (Statement)
import Network.Wai.Application.Static
import Servant.Server.StaticFiles
import Servant (ServerT, Raw)

import qualified Data.Binary.Builder as Builder
import qualified Network.Wai as Wai
import qualified WaiAppStatic.Types as Static
import qualified Data.Text as Text

runQuery :: SQL.Context -> Statement a b -> a -> IO b
runQuery ctx stm val = SQL.runAndThrow ctx $ SQL.query stm val

fromPieces :: Static.Pieces -> (Text, Text)
fromPieces ps =
  let
    (fs,ns) = splitAt (length ps - 1) ps
  in
    ( Text.intercalate "/" $ fmap Static.fromPiece fs
    , Static.fromPiece $ head ns
    )

fromFileDB :: SQL.Context -> FileDB -> Static.File
fromFileDB ctx file =
  let
    key = (fileDB_folder file, fileDB_name file)

    streamingBody :: Wai.StreamingBody
    streamingBody send flush = do
      contents <- runQuery ctx SQL.Files.selectFileContents key
      send (Builder.fromByteString contents)
      flush

  in
    Static.File
      { Static.fileGetSize =
          fromIntegral $ fileDB_size file
      , Static.fileToResponse = \status headers ->
          Wai.responseStream status headers streamingBody
      , Static.fileName =
          Static.unsafeToPiece $ fileDB_name file
      , Static.fileGetHash =
          pure $ Just $ fileDB_hash file
      , Static.fileGetModified =
          Just $ fileDB_modifiedAt file
      }

lookupFile :: SQL.Context -> Static.Pieces -> IO Static.LookupResult
lookupFile ctx pieces = do
  let (folder,file) = fromPieces pieces
  if Text.null file
  -- Folder
  then do
    filesDB <- runQuery ctx SQL.Files.selectFolderFiles folder
    let files = fmap (Right . fromFileDB ctx) filesDB
    foldersDB <- runQuery ctx SQL.Files.selectFolderSubfolders folder
    let -- this is so file listings work, we can remove the hack when doing a JSON listing
        addSlash t = t <> "/"
        folders = fmap (Left . Static.unsafeToPiece . addSlash) foldersDB
        result = folders <> files
    if null result
    then pure Static.LRNotFound
    else pure $ Static.LRFolder $ Static.Folder result
  -- File
  else do
    result <- runQuery ctx SQL.Files.selectFile (folder, file)
    case result of
      Nothing -> pure Static.LRNotFound
      Just fileDB -> pure $ Static.LRFile $ fromFileDB ctx fileDB

staticSettings :: SQL.Context -> StaticSettings
staticSettings ctx = (defaultFileServerSettings "XXX")
  { Static.ssLookupFile = lookupFile ctx
  , Static.ssUseHash = True
  , Static.ssIndices = []
  , Static.ssAddTrailingSlash = True
  }

serveSQLFileServer :: SQL.Context -> ServerT Raw m
serveSQLFileServer ctx = serveDirectoryWith (staticSettings ctx)
