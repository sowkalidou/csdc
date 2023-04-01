{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Files
  ( selectFile,
    selectFileContents,
    upsertFile,
    selectFolderFiles,
    selectFolderSubfolders,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.Types.File (FileDB (..), NewFileDB (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Decoders qualified as Decoders
import Hasql.Statement (Statement (..))

--------------------------------------------------------------------------------
-- Files

selectFile :: Statement (Text, Text) (Maybe FileDB)
selectFile = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT folder, name, size, hash, modified_at",
          "FROM files",
          "WHERE folder = $1 AND name = $2"
        ]

    encoder =
      contramap fst Encoder.text
        <> contramap snd Encoder.text

    decoder = Decoder.rowMaybe $ do
      folder <- Decoder.text
      name <- Decoder.text
      size <- Decoder.int
      hash <- Decoder.bytea
      modifiedAt <- Decoder.posixTime
      pure FileDB {..}

selectFileContents :: Statement (Text, Text) ByteString
selectFileContents = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT contents",
          "FROM files",
          "WHERE folder = $1 AND name = $2"
        ]

    encoder =
      contramap fst Encoder.text
        <> contramap snd Encoder.text

    decoder = Decoder.singleRow Decoder.bytea

upsertFile :: Statement NewFileDB ()
upsertFile = Statement sql encoder Decoders.noResult True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO files (folder,name,contents,size,hash,modified_at)",
          "VALUES ($1,$2,$3,$4,$5,NOW())",
          "ON CONFLICT (folder,name)",
          "DO UPDATE SET contents = $3, size = $4, hash = $5, modified_at = NOW()"
        ]

    encoder =
      contramap (.folder) Encoder.text
        <> contramap (.name) Encoder.text
        <> contramap (.contents) Encoder.bytea
        <> contramap (.size) Encoder.int
        <> contramap (.hash) Encoder.bytea

--------------------------------------------------------------------------------
-- Folders

selectFolderFiles :: Statement Text [FileDB]
selectFolderFiles = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT folder, name, size, hash, modified_at",
          "FROM files",
          "WHERE folder = $1",
          "ORDER BY name"
        ]

    encoder = Encoder.text

    decoder = Decoder.rowList $ do
      folder <- Decoder.text
      name <- Decoder.text
      size <- Decoder.int
      hash <- Decoder.bytea
      modifiedAt <- Decoder.posixTime
      pure FileDB {..}

selectFolderSubfolders :: Statement Text [Text]
selectFolderSubfolders = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT DISTINCT ON (result)",
          "regexp_replace(regexp_replace(folder, $1, ''), '/(.)+', '') as result",
          "FROM files",
          "WHERE folder LIKE $2",
          "ORDER BY result"
        ]

    toFolder t = t <> "/"

    toLike t = t <> "/%"

    encoder =
      contramap toFolder Encoder.text
        <> contramap toLike Encoder.text

    decoder = Decoder.rowList Decoder.text
