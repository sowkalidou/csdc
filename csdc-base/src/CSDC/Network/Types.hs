module CSDC.Network.Types
  ( -- Entities
    Person (..)
  , Unit (..)
    -- Relations
  , Member (..)
  , Subpart (..)
  ) where

import CSDC.Aeson (JSON (..))
import CSDC.Data.Id (Id)

import qualified CSDC.Auth.ORCID as ORCID

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: Text
  , person_orcid :: ORCID.Id
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Person

data Unit = Unit
  { unit_name :: Text
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Unit

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { member_person :: Id Person
  , member_unit :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Member

data Subpart = Subpart
  { subpart_child :: Id Unit
  , subpart_parent :: Id Unit
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Subpart
