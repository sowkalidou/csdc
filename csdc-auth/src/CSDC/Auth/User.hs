{-# LANGUAGE TemplateHaskell #-}

module CSDC.Auth.User
  ( User (..)
  ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

-- | The current user of the server API.
newtype User user = User user
  deriving (Show, Eq)

deriveJSON defaultOptions ''User
