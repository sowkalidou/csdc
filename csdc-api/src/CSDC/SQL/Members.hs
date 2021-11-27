{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Members
  ( selectByPerson
  , selectByUnit
  , insert
  , delete
  , deleteUnit
  ) where

import CSDC.DAO.Types

import qualified CSDC.SQL.Decoder as Decoder
import qualified CSDC.SQL.Encoder as Encoder

import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

import qualified Data.ByteString.Char8 as ByteString

selectByPerson :: Statement (Id Person) [PersonMember]
selectByPerson = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT members.id, unit, units.name, units.description, units.chair, units.created_at"
      , "FROM members"
      , "JOIN units ON units.id = unit"
      , "WHERE person = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      personMember_member <- Decoder.id
      personMember_id <- Decoder.id
      personMember_unit <- do
        unit_name <- Decoder.text
        unit_description <- Decoder.text
        unit_chair <- Decoder.id
        unit_createdAt <- Decoder.timestamptz
        pure Unit {..}
      pure PersonMember {..}

selectByUnit :: Statement (Id Unit) [UnitMember]
selectByUnit = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT members.id, person, persons.name, persons.description, persons.orcid, persons.created_at"
      , "FROM members"
      , "JOIN persons ON persons.id = person"
      , "WHERE unit = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      unitMember_member <- Decoder.id
      unitMember_id <- Decoder.id
      unitMember_person <- do
        person_name <- Decoder.text
        person_description <- Decoder.text
        person_orcid <- Decoder.orcidId
        person_createdAt <- Decoder.timestamptz
        pure Person {..}
      pure UnitMember {..}

insert :: Statement NewMember (Id Member)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO members (person, unit)"
      , "VALUES ($1, $2)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newMember_person Encoder.id) <>
      (contramap newMember_unit Encoder.id)

    decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Member) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM members"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

deleteUnit :: Statement (Id Unit) ()
deleteUnit = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM members"
      , "WHERE unit = $1"
      ]

    encoder = Encoder.id
    decoder = Decoder.noResult

