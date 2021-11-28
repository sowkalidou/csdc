{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Units
  ( select
  , selectByChair
  , search
  , insert
  , update
  , delete
  ) where

import CSDC.Prelude

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

select :: Statement (Id Unit) (Maybe Unit)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT name, description, chair, created_at"
      , "FROM units"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $ do
      unit_name <- Decoder.text
      unit_description <- Decoder.text
      unit_chair <- Decoder.id
      unit_createdAt <- Decoder.timestamptz
      pure Unit {..}

selectByChair :: Statement (Id Person) [WithId Unit]
selectByChair = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, name, description, chair, created_at"
      , "FROM units"
      , "WHERE chair = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      withId_id <- Decoder.id
      withId_value <- do
        unit_name <- Decoder.text
        unit_description <- Decoder.text
        unit_chair <- Decoder.id
        unit_createdAt <- Decoder.timestamptz
        pure Unit {..}
      pure WithId {..}

search :: Statement [Text] [SearchResult (Id Unit)]
search = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, name"
      , "FROM units"
      , "WHERE name ILIKE ALL ($1)"
      ]

    encoder = Encoder.textList

    decoder = Decoder.rowList $ do
      searchResult_id <- Decoder.id
      searchResult_name <- Decoder.text
      pure SearchResult {..}

insert :: Statement Unit (Id Unit)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO units (name, description, chair)"
      , "VALUES ($1, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      (contramap unit_name Encoder.text) <>
      (contramap unit_description Encoder.text) <>
      (contramap unit_chair Encoder.id)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Unit, UnitUpdate) ()
update = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE units"
      , "SET name = $2, description = $3"
      , "WHERE id = $1"
      ]

    encoder =
      (contramap fst Encoder.id) <>
      (contramap (unitUpdate_name . snd) Encoder.text) <>
      (contramap (unitUpdate_description . snd) Encoder.text)

    decoder = Decoder.noResult

delete :: Statement (Id Unit) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM units"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult
