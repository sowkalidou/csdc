{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Persons
  ( select,
    check,
    insert,
    search,
    update,
    updateImage,
    delete,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import CSDC.SQL.QQ
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Data.Password.Bcrypt (Bcrypt, PasswordHash (..))
import Hasql.Statement (Statement (..))

select :: Statement (Id Person) (Maybe Person)
select = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT name, description, email, image, created_at",
          "FROM persons",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowMaybe $ do
      name <- Decoder.text
      description <- Decoder.text
      email <- Decoder.text
      image <- Decoder.text
      createdAt <- Decoder.posixTime
      pure Person {..}

check :: Statement Text (Maybe (Id Person, PasswordHash Bcrypt))
check = Statement sql encoder decoder True
  where
    sql =
      [sqlqq|
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
    sql =
      ByteString.unlines
        [ "SELECT id, name",
          "FROM persons",
          "WHERE name ILIKE ALL ($1)"
        ]

    encoder = Encoder.textList

    decoder = Decoder.rowList $ do
      id <- Decoder.id
      name <- Decoder.text
      pure SearchResult {..}

insert :: Statement NewPerson (Id Person)
insert = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO persons (name, description, email, password_hash, image)",
          "VALUES ($1, $2, $3, $4, $5)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.name) Encoder.text)
        <> (contramap (.description) Encoder.text)
        <> (contramap (.email) Encoder.text)
        <> (contramap (unPasswordHash . (.password)) Encoder.text)
        <> (contramap (.image) Encoder.text)

    decoder = Decoder.singleRow Decoder.id

update :: Statement (Id Person, PersonUpdate) ()
update = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE persons",
          "SET name = $2, description = $3",
          "WHERE id = $1"
        ]

    encoder =
      (contramap fst Encoder.id)
        <> (contramap ((.name) . snd) Encoder.text)
        <> (contramap ((.description) . snd) Encoder.text)

    decoder = Decoder.noResult

updateImage :: Statement (Id Person, Text) ()
updateImage = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "UPDATE persons",
          "SET image = $2",
          "WHERE id = $1"
        ]

    encoder =
      contramap fst Encoder.id
        <> contramap snd Encoder.text

    decoder = Decoder.noResult

delete :: Statement (Id Person) ()
delete = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM persons",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult
