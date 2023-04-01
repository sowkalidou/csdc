{-# LANGUAGE OverloadedStrings #-}

module CSDC.Types.File
  ( Base64File (..),
    base64FileFromByteString,
    File (..),
    fromBase64File,
    NewFileDB (..),
    toNewFileDB,
    FileDB (..),
    FileUI (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as Lazy
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

-- | A file encoded in base 64, as encoded in a HTML canvas, with name.
data Base64File = Base64File
  { name :: Text,
    contents :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

base64FileFromByteString :: Text -> Lazy.ByteString -> Base64File
base64FileFromByteString name contents =
  Base64File
    { name = name,
      contents =
        "image/svg+xml;base64,"
          <> decodeUtf8 (Base64.encode (Lazy.toStrict contents))
    }

parseBase64Contents :: Text -> ByteString
parseBase64Contents =
  Base64.decodeLenient
    . encodeUtf8
    . fromMaybe (error "Could not extract base 64 prefix.")
    . Text.stripPrefix ";base64,"
    . Text.dropWhile (/= ';')

-- | A file, with its name and contents.
data File = File
  { name :: Text,
    contents :: ByteString
  }
  deriving (Show, Eq)

fromBase64File :: Base64File -> File
fromBase64File (Base64File name contents) =
  File
    { name = name,
      contents = parseBase64Contents contents
    }

-- | A file to be stored in the database.
data NewFileDB = NewFileDB
  { folder :: Text,
    name :: Text,
    contents :: ByteString,
    size :: Int,
    hash :: ByteString
  }
  deriving (Show, Eq)

toNewFileDB :: MonadIO m => Text -> File -> m NewFileDB
toNewFileDB folder (File name contents) = do
  pure
    NewFileDB
      { folder = folder,
        name = name,
        contents = contents,
        size = ByteString.length contents,
        hash = MD5.hash contents
      }

-- | A file as stored in the database. It doesn't contain the contents so that
-- file listings are efficient.
data FileDB = FileDB
  { folder :: Text,
    name :: Text,
    size :: Int,
    hash :: ByteString,
    modifiedAt :: POSIXTime
  }
  deriving (Show, Eq)

-- | A file displayed in the UI.
data FileUI = FileUI
  { path :: Text,
    name :: Text,
    size :: Int,
    modifiedAt :: POSIXTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
