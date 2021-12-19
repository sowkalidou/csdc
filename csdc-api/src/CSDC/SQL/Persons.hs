{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module CSDC.SQL.Persons
  ( select
  , check
  , insert
  , search
  , update
  , updateImage
  , delete
  ) where

import CSDC.Prelude
import CSDC.SQL.QQ

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Password.Bcrypt (PasswordHash (..), Bcrypt)
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

select :: Statement (Id Person) (Maybe Person)
select = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT name, description, email, image, created_at"
      , "FROM persons"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $ do
      person_name <- Decoder.text
      person_description <- Decoder.text
      person_email <- Decoder.text
      person_image <- Decoder.text
      person_createdAt <- Decoder.posixTime
      pure Person {..}

check :: Statement Text (Maybe (Id Person, PasswordHash Bcrypt))
check = Statement sql encoder decoder True
  where
    sql = [sqlqq|
      SELECT id, password_hash
      FROM persons
      WHERE email = $1
      |]

    encoder =
      Encoder.text

    decoder = Decoder.rowMaybe $ do
      personId <- Decoder.id
      passwordHash <- Decoder.text
      pure (personId, PasswordHash passwordHash)

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
      [ "INSERT INTO persons (name, description, email, password_hash, image)"
      , "VALUES ($1, $2, $3, $4, $5)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newPerson_name Encoder.text) <>
      (contramap newPerson_description Encoder.text) <>
      (contramap newPerson_email Encoder.text) <>
      (contramap (unPasswordHash . newPerson_password) Encoder.text) <>
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
