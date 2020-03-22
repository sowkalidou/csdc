{-# LANGUAGE DeriveGeneric #-}

module CSDC.Network.Types where

import CSDC.Data.Id (Id)

import Data.Aeson
  ( Options
  , ToJSON (..)
  , FromJSON (..)
  , defaultOptions
  , fieldLabelModifier
  , genericToJSON
  , genericParseJSON
  )
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Fundamental types

data Person = Person
  { person_name :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Person where
  toJSON = genericToJSON options

instance FromJSON Person where
  parseJSON = genericParseJSON options

data Unit = Unit
  { unit_name :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Unit where
  toJSON = genericToJSON options

instance FromJSON Unit where
  parseJSON = genericParseJSON options

--------------------------------------------------------------------------------
-- Fundamental relations

data Member = Member
  { member_person :: Id Person
  , member_unit :: Id Unit
  } deriving (Show, Eq, Generic)

instance ToJSON Member where
  toJSON = genericToJSON options

instance FromJSON Member where
  parseJSON = genericParseJSON options

data Subpart = Subpart
  { subpart_child :: Id Unit
  , subpart_parent :: Id Unit
  } deriving (Show, Eq, Generic)

instance ToJSON Subpart where
  toJSON = genericToJSON options

instance FromJSON Subpart where
  parseJSON = genericParseJSON options

--------------------------------------------------------------------------------
-- JSON

options :: Options
options = defaultOptions
  { fieldLabelModifier = tail . dropWhile (/= '_')
  }
