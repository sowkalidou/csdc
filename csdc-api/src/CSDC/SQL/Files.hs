{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Files
  ( select
  , upsert
  ) where

import CSDC.Prelude
import CSDC.Data.File (FileDB (..))

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString
import qualified Hasql.Decoders as Decoders

select :: Statement Text (Maybe FileDB)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT filepath, contents, size, hash, modified_at"
      , "FROM files"
      , "WHERE filepath = $1"
      ]

    encoder = Encoder.text

    decoder = Decoder.rowMaybe $ do
      fileDB_filepath <- Decoder.text
      fileDB_contents <- Decoder.bytea
      fileDB_size <- Decoder.int
      fileDB_hash <- Decoder.bytea
      fileDB_modifiedAt <- Decoder.ctime
      pure FileDB {..}

upsert :: Statement FileDB ()
upsert = Statement sql encoder Decoders.noResult True
  where
    sql = ByteString.unlines
      [ "INSERT INTO files (filepath, contents, size, hash, modified_at)"
      , "VALUES ($1,$2,$3,$4,$5)"
      , "ON CONFLICT (filepath)"
      , "DO UPDATE SET contents = $2, size = $3, hash = $4, modified_at = $5"
      ]

    encoder =
      contramap fileDB_filepath Encoder.text <>
      contramap fileDB_contents Encoder.bytea <>
      contramap fileDB_size Encoder.int <>
      contramap fileDB_hash Encoder.bytea <>
      contramap fileDB_modifiedAt Encoder.ctime

