{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.SQL.Subparts
  ( selectByChild
  , selectByParent
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

selectByChild :: Statement (Id Unit) [UnitSubpart]
selectByChild = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT subparts.id, parent, units.name, units.description, units.chair, units.created_at"
      , "FROM subparts"
      , "JOIN units ON units.id = parent"
      , "WHERE child = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      unitSubpart_subpart <- Decoder.id
      unitSubpart_id <- Decoder.id
      unitSubpart_unit <- do
        unit_name <- Decoder.text
        unit_description <- Decoder.text
        unit_chair <- Decoder.id
        unit_createdAt <- Decoder.timestamptz
        pure Unit {..}
      pure UnitSubpart {..}

selectByParent :: Statement (Id Unit) [UnitSubpart]
selectByParent = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "SELECT subparts.id, child, units.name, units.description, units.chair, units.created_at"
      , "FROM subparts"
      , "JOIN units ON units.id = child"
      , "WHERE parent = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      unitSubpart_subpart <- Decoder.id
      unitSubpart_id <- Decoder.id
      unitSubpart_unit <- do
        unit_name <- Decoder.text
        unit_description <- Decoder.text
        unit_chair <- Decoder.id
        unit_createdAt <- Decoder.timestamptz
        pure Unit {..}
      pure UnitSubpart {..}

insert :: Statement NewSubpart (Id Subpart)
insert = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "INSERT INTO subparts (child, parent)"
      , "VALUES ($1, $2)"
      , "RETURNING id"
      ]

    encoder =
      (contramap newSubpart_child Encoder.id) <>
      (contramap newSubpart_parent Encoder.id)

    decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Subpart) ()
delete = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM subparts"
      , "WHERE id = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult

deleteUnit :: Statement (Id Unit) ()
deleteUnit = Statement sql encoder decoder True
  where
    sql = ByteString.unlines
      [ "DELETE FROM subparts"
      , "WHERE child = $1 OR parent = $1"
      ]

    encoder = Encoder.id

    decoder = Decoder.noResult
