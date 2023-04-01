module CSDC.FileServer
  ( serveSQLFileServer,
  )
where

import CSDC.SQL qualified as SQL
import CSDC.SQL.Files qualified as SQL.Files
import CSDC.Types.File (FileDB (..))
import Data.Binary.Builder qualified as Builder
import Data.ByteString.Base64 qualified as Base64
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C.Types (CTime (..))
import Hasql.Statement (Statement)
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
import Servant (Raw, ServerT)
import Servant.Server.StaticFiles
import WaiAppStatic.Types qualified as Static

runQuery :: SQL.Context -> Statement a b -> a -> IO b
runQuery ctx stm val = SQL.runAndThrow ctx $ SQL.query stm val

fromPieces :: Static.Pieces -> (Text, Text)
fromPieces ps =
  let (fs, ns) = splitAt (length ps - 1) ps
   in ( Text.intercalate "/" $ fmap Static.fromPiece fs,
        Static.fromPiece $ head ns
      )

fromFileDB :: SQL.Context -> FileDB -> Static.File
fromFileDB ctx file =
  let key = (file.folder, file.name)

      streamingBody :: Wai.StreamingBody
      streamingBody send flush = do
        contents <- runQuery ctx SQL.Files.selectFileContents key
        send (Builder.fromByteString contents)
        flush
   in Static.File
        { Static.fileGetSize =
            fromIntegral file.size,
          Static.fileToResponse = \status headers ->
            Wai.responseStream status headers streamingBody,
          Static.fileName =
            Static.unsafeToPiece file.name,
          Static.fileGetHash =
            pure $ Just $ Base64.encode file.hash,
          Static.fileGetModified =
            Just $ CTime $ floor file.modifiedAt
        }

lookupFile :: SQL.Context -> Static.Pieces -> IO Static.LookupResult
lookupFile ctx pieces = do
  let (folder, file) = fromPieces pieces
  if Text.null file
    then -- Folder
    do
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
    else -- File
    do
      result <- runQuery ctx SQL.Files.selectFile (folder, file)
      case result of
        Nothing -> pure Static.LRNotFound
        Just fileDB -> pure $ Static.LRFile $ fromFileDB ctx fileDB

staticSettings :: SQL.Context -> StaticSettings
staticSettings ctx =
  (defaultFileServerSettings "XXX")
    { Static.ssLookupFile = lookupFile ctx,
      Static.ssUseHash = True,
      Static.ssIndices = [],
      Static.ssAddTrailingSlash = True
    }

serveSQLFileServer :: SQL.Context -> ServerT Raw m
serveSQLFileServer ctx = serveDirectoryWith (staticSettings ctx)
