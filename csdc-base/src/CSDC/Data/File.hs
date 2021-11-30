{-# LANGUAGE OverloadedStrings #-}

module CSDC.Data.File
  ( Base64File
  , fromBase64File
  , File (..)
  , toFileDB
  , FileDB (..)
  ) where

import CSDC.Aeson (JSON (..))

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.C.Types (CTime (..))
import GHC.Generics (Generic)
import System.Posix.Types (EpochTime)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as Text

-- | A file encoded in base 64, as encoded in a HTML canvas, with name.
data Base64File = Base64File
  { base64File_name :: Text
  , base64File_contents :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Base64File

parseBase64Contents :: Text -> ByteString
parseBase64Contents =
  Base64.decodeLenient .
  encodeUtf8 .
  fromMaybe (error "Could not extract base 64 prefix.") .
  Text.stripPrefix ";base64," .
  Text.dropWhile (/= ';')

-- | A file, with its name and contents.
data File = File
  { file_name :: Text
  , file_contents :: ByteString
  } deriving (Show, Eq)

fromBase64File :: Base64File -> File
fromBase64File (Base64File name contents) = File
  { file_name = name
  , file_contents = parseBase64Contents contents
  }

-- | A file as stored in the database.
data FileDB = FileDB
  { fileDB_filepath :: Text
  , fileDB_contents :: ByteString
  , fileDB_size :: Int
  , fileDB_hash :: ByteString
  , fileDB_modifiedAt :: EpochTime
  } deriving (Show, Eq)

toFileDB :: MonadIO m => Text -> File -> m FileDB
toFileDB folder (File name contents) = do
  now <- CTime <$> floor <$> liftIO getPOSIXTime
  pure FileDB
    { fileDB_filepath = folder <> "/" <> name
    , fileDB_contents = contents
    , fileDB_size = ByteString.length contents
    , fileDB_hash = MD5.hash contents
    , fileDB_modifiedAt = now
    }


