{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module CSDC.SQL.Subparts
  ( selectByChild,
    selectByParent,
    insert,
    delete,
    deleteUnit,
  )
where

import CSDC.Prelude
import CSDC.SQL.Decoder qualified as Decoder
import CSDC.SQL.Encoder qualified as Encoder
import Data.ByteString.Char8 qualified as ByteString
import Data.Functor.Contravariant (Contravariant (..))
import Hasql.Statement (Statement (..))

selectByChild :: Statement (Id Unit) [UnitSubpart]
selectByChild = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT subparts.id, parent, units.name, units.description, units.chair, units.image, units.created_at",
          "FROM subparts",
          "JOIN units ON units.id = parent",
          "WHERE child = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      subpartId <- Decoder.id
      unitId <- Decoder.id
      unit <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure UnitSubpart {..}

selectByParent :: Statement (Id Unit) [UnitSubpart]
selectByParent = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "SELECT subparts.id, child, units.name, units.description, units.chair, units.image, units.created_at",
          "FROM subparts",
          "JOIN units ON units.id = child",
          "WHERE parent = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.rowList $ do
      subpartId <- Decoder.id
      unitId <- Decoder.id
      unit <- do
        name <- Decoder.text
        description <- Decoder.text
        chairId <- Decoder.id
        image <- Decoder.text
        createdAt <- Decoder.posixTime
        pure Unit {..}
      pure UnitSubpart {..}

insert :: Statement NewSubpart (Id Subpart)
insert = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "INSERT INTO subparts (child, parent)",
          "VALUES ($1, $2)",
          "RETURNING id"
        ]

    encoder =
      (contramap (.childId) Encoder.id)
        <> (contramap (.parentId) Encoder.id)

    decoder = Decoder.singleRow Decoder.id

delete :: Statement (Id Subpart) ()
delete = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM subparts",
          "WHERE id = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult

deleteUnit :: Statement (Id Unit) ()
deleteUnit = Statement sql encoder decoder True
  where
    sql =
      ByteString.unlines
        [ "DELETE FROM subparts",
          "WHERE child = $1 OR parent = $1"
        ]

    encoder = Encoder.id

    decoder = Decoder.noResult
