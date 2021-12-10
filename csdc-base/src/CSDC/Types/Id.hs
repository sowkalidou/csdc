{-# LANGUAGE OverloadedStrings #-}

module CSDC.Types.Id
  ( Id (..)
  , newId
  , WithId (..)
  ) where

import CSDC.Aeson (JSON (..))

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Web.Internal.HttpApiData (FromHttpApiData (..))

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

-- | A unique identifier for some type.
newtype Id a = Id { getId :: UUID }
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

instance FromHttpApiData (Id a) where
  parseUrlPiece txt =
    case UUID.fromText txt of
      Nothing -> Left $ "Invalid UUID: " <> txt
      Just uuid -> pure $ Id uuid

newId :: MonadIO m => m (Id a)
newId = Id <$> liftIO UUID.nextRandom

data WithId a = WithId
  { withId_id :: Id a
  , withId_value :: a
  } deriving (Show, Eq, Ord, Generic)
    deriving (FromJSON, ToJSON) via JSON (WithId a)
