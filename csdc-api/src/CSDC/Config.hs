{-# LANGUAGE StrictData #-}

module CSDC.Config
  ( Config (..)
  , readConfig
  , showConfig
  ) where

import CSDC.Prelude

import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as ByteString

data Config = Config
  { config_port :: Int
  , config_path :: FilePath
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

readConfig :: MonadIO m => FilePath -> m (Maybe Config)
readConfig path = liftIO $ decodeFileStrict path

showConfig :: MonadIO m => Config -> m ()
showConfig config =
  let
    str = ByteString.unpack $ encodePretty config
  in
    liftIO $ putStrLn str
