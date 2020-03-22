module CSDC.Data.Id
  ( Id (..)
  , zero
  , next
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Web.Internal.HttpApiData (FromHttpApiData)

-- | A unique identifier for some type.
newtype Id a = Id Int
  deriving newtype (Show, Eq, ToJSON, FromJSON, FromHttpApiData)

zero :: Id a
zero = Id 0

next :: Id a -> Id a
next (Id a) = Id (a + 1)
