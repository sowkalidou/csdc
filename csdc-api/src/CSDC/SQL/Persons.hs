{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Persons
  ( select
  , insert
  , update
  , delete
  , selectORCID
  ) where

import CSDC.DAO.Types
import CSDC.Data.Id (Id (..))

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
      person_createdAt <- Decoder.timestamptz
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

insert :: Statement NewPerson (Id Person)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO persons (name, description, orcid)"
      , "VALUES ($1, $2, $3)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newPerson_name Encoder.text) <>
      (contramap newPerson_description Encoder.text) <>
      (contramap newPerson_orcid Encoder.orcidId)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Person, PersonUpdate) ()
update = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "UPDATE persons"
      , "SET name = $2, description = $3, image = $4"
      , "WHERE id = $1"
      ]

    encoder =
      (contramap fst Encoder.id) <>
      (contramap (personUpdate_name . snd) Encoder.text) <>
      (contramap (personUpdate_description . snd) Encoder.text) <>
      (contramap (personUpdate_image . snd) Encoder.textNullable)

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
