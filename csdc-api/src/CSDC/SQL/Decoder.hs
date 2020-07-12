module CSDC.SQL.Decoder
  ( -- * Base types
    text
    -- * Local types
  , id
  , orcidId
    -- * Reexport
  , Decoders.rowList
  , Decoders.rowMaybe
  , Decoders.singleRow
  , Decoders.noResult
  ) where

import CSDC.Prelude
import Prelude hiding (id)

import qualified CSDC.Auth.ORCID as ORCID

import Data.Text (Text)
import Hasql.Decoders (Row, column, nonNullable)

import qualified Hasql.Decoders as Decoders

--------------------------------------------------------------------------------
-- Base types

text :: Row Text
text = column (nonNullable Decoders.text)

--------------------------------------------------------------------------------
-- Local types

id :: Row (Id a)
id = Id . fromIntegral <$> column (nonNullable Decoders.int4)

orcidId :: Row ORCID.Id
orcidId = ORCID.Id <$> column (nonNullable Decoders.text)
