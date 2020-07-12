module CSDC.SQL.Encoder
  ( -- * Base types
    text
  , id
    -- * Local types
  , orcidId
  ) where

import CSDC.Prelude
import Prelude hiding (id)

import qualified CSDC.Auth.ORCID as ORCID

import Data.Functor.Contravariant (Contravariant (..))
import Data.Text (Text)
import Hasql.Encoders (Params, param, nonNullable)

import qualified Hasql.Encoders as Encoders

--------------------------------------------------------------------------------
-- Base types

text :: Params Text
text = param (nonNullable Encoders.text)

--------------------------------------------------------------------------------
-- Local types

id :: Params (Id a)
id =
  contramap (fromIntegral . getId) $
  param (nonNullable Encoders.int4)

orcidId :: Params ORCID.Id
orcidId =
  contramap ORCID.getId $
  param (nonNullable Encoders.text)
