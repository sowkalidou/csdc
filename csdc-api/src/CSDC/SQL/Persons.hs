{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Persons
  ( select
  , insert
  , search
  , update
  , updateImage
  , delete
  , selectORCID
  ) where

import CSDC.Prelude

import qualified CSDC.Auth.ORCID as ORCID
import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

select :: Statement (Id Person) (Maybe Person)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT name, description, orcid, image, created_at"
      , "FROM persons"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $ do
      person_name <- Decoder.text
      person_description <- Decoder.text
      person_orcid <- Decoder.orcidId
      person_image <- Decoder.textNullable
      person_createdAt <- Decoder.posixTime
      pure Person {..}

selectORCID :: Statement ORCID.Id (Maybe (Id Person))
selectORCID = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id"
      , "FROM persons"
      , "WHERE orcid = $1"
      ]

    encoder = Encoder.orcidId

    decoder = Decoder.rowMaybe Decoder.id

search :: Statement [Text] [SearchResult (Id Person)]
search = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT id, name"
      , "FROM persons"
      , "WHERE name ILIKE ALL ($1)"
      ]

    encoder = Encoder.textList

    decoder = Decoder.rowList $ do
      searchResult_id <- Decoder.id
      searchResult_name <- Decoder.text
      pure SearchResult {..}

insert :: Statement NewPerson (Id Person)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO persons (name, description, orcid, image)"
      , "VALUES ($1, $2, $3, $4)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newPerson_name Encoder.text) <>
      (contramap newPerson_description Encoder.text) <>
      (contramap newPerson_orcid Encoder.orcidId) <>
      (contramap newPerson_image Encoder.text)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Person, PersonUpdate) ()
update = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE persons"
      , "SET name = $2, description = $3"
      , "WHERE id = $1"
      ]

    encoder =
      (contramap fst Encoder.id) <>
      (contramap (personUpdate_name . snd) Encoder.text) <>
      (contramap (personUpdate_description . snd) Encoder.text)

    decoder = Decoder.noResult

updateImage :: Statement (Id Person, Text) ()
updateImage = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE persons"
      , "SET image = $2"
      , "WHERE id = $1"
      ]

    encoder =
      contramap fst Encoder.id <>
      contramap snd Encoder.text

    decoder = Decoder.noResult

delete :: Statement (Id Person) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM persons"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult
