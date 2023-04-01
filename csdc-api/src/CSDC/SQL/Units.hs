{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Units
  ( select,
    selectByChair,
    search,
    searchUnits,
    insert,
    update,
    updateImage,
    updateChair,
    delete,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

select :: Statement (Id Unit) (Maybe Unit)
select = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT name, description, chair, image, created_at",
          "FROM units",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $ do
      name <- Decoder.text
      description <- Decoder.text
      chairId <- Decoder.id
      image <- Decoder.text
      createdAt <- Decoder.posixTime
      pure Unit {..}

selectByChair :: Statement (Id Person) [WithId Unit]
selectByChair = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT id, name, description, chair, image, created_at",
          "FROM units",
          "WHERE chair = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      value <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure WithId {..}

search :: Statement [Text] [SearchResult (Id Unit)]
search = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT id, name",
          "FROM units",
          "WHERE name ILIKE ALL ($1)"
        ]

    encoder = Encoder.textList

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      name <- Decoder.text
      pure SearchResult {..}

searchUnits :: Statement [Text] [WithId Unit]
searchUnits = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT id, name, description, chair, image, created_at",
          "FROM units",
          "WHERE name ILIKE ALL ($1)"
        ]

    encoder = Encoder.textList

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      value <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure WithId {..}

insert :: Statement Unit (Id Unit)
insert = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO units (name, description, chair, image)",
          "VALUES ($1, $2, $3, $4)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.name) Encoder.text)
        <> (contramap (.description) Encoder.text)
        <> (contramap (.chairId) Encoder.id)
        <> (contramap (.image) Encoder.text)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Unit, UnitUpdate) ()
update = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE units",
          "SET name = $2, description = $3",
          "WHERE id = $1"
        ]

    encoder =
      (contramap fst Encoder.id)
        <> (contramap ((.name) . snd) Encoder.text)
        <> (contramap ((.description) . snd) Encoder.text)

    decoder = Decoder.noResult

updateImage :: Statement (Id Unit, Text) ()
updateImage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE units",
          "SET image = $2",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.text

    decoder = Decoder.noResult

updateChair :: Statement (Id Unit, Id Person) ()
updateChair = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE units",
          "SET chair = $2",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.id

    decoder = Decoder.noResult

delete :: Statement (Id Unit) ()
delete = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM units",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult
