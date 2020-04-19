{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

--------------------------------------------------------------------------------
-- References
--
-- https://members.orcid.org/api/about-public-api
-- https://members.orcid.org/api/integrate/orcid-sign-in
-- https://members.orcid.org/api/oauth/3legged-oauth
-- https://members.orcid.org/api/tutorial/read-orcid-records
--
--------------------------------------------------------------------------------

module CSDC.ORCID
  ( -- * Configuration
    Config (..)
  , Endpoint (..)
    -- * Authentication Middleware
  , authenticationMiddleware
    -- * User Identity
  , Token (..)
  , getToken
    -- * User Record
  , getUserRecord
  ) where

import CSDC.ORCID.OAuth2 (OAuth2 (..), getAccessToken)

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , withObject
  , withText
  , object
  , (.:)
  , (.:?)
  , (.=)
  )
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack (HasCallStack)
import Network.OAuth.OAuth2
  ( AccessToken
  , RefreshToken
  , authGetJSON
  )
import Network.Wai (Middleware, Request)
import Network.Wai.Middleware.Auth
  ( AuthSettings
  , defaultAuthSettings
  , mkAuthMiddleware
  , setAuthProviders
  , setAuthSessionAge
  )
import Network.Wai.Middleware.Auth.Provider (Provider (..), ProviderInfo (..))
import URI.ByteString (parseURI, strictURIParserOptions)

import qualified Network.HTTP.Client.TLS as HTTP.TLS
import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------
-- Configuration

data Config = Config
  { config_id :: Text
  , config_secret :: Text
  , config_endpoint :: Endpoint
  } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config <$>
      (o .: "id") <*>
      (o .: "secret") <*>
      (o .: "endpoint")

instance ToJSON Config where
  toJSON (Config uid secret endpoint) = object
    [ "id" .= uid
    , "secret" .= secret
    , "endpoint" .= endpoint
    ]

data Endpoint = Production | Sandbox
  deriving (Show, Eq)

instance FromJSON Endpoint where
  parseJSON = withText "Endpoint" $ \case
    "production" -> pure Production
    "sandbox"-> pure Sandbox
    _ -> fail "Endpoint must be 'production' or 'sandbox'."

instance ToJSON Endpoint where
  toJSON Production = String "production"
  toJSON Sandbox = String "sandbox"

makeHost :: Endpoint -> Text
makeHost Production = "https://orcid.org"
makeHost Sandbox = "https://sandbox.orcid.org"

makeAPIHost :: Endpoint -> Text
makeAPIHost Production = "https://pub.orcid.org"
makeAPIHost Sandbox = "https://pub.sandbox.orcid.org"

--------------------------------------------------------------------------------
-- OAuth

oauth2 :: Config -> OAuth2
oauth2 config = OAuth2
  { oa2ClientId =
      config_id config

  , oa2ClientSecret =
      config_secret config

  , oa2AuthorizeEndpoint =
      makeHost (config_endpoint config) <> "/oauth/authorize"

  , oa2AccessTokenEndpoint =
      makeHost (config_endpoint config) <> "/oauth/token"

  , oa2Scope =
      Just [ "/authenticate" ]

  , oa2ProviderInfo =
      ProviderInfo
        { providerTitle =
            "ORCID"
        , providerLogoUrl =
            "https://orcid.org/sites/default/files/images/orcid_16x16.png"
        , providerDescr =
           "Login using your ORCID account."
        }
  }

authSettings :: Config -> AuthSettings
authSettings config =
  let
    provider = Provider (oauth2 config)
    providers = HashMap.singleton "oauth2_alternative" provider
  in
    setAuthProviders providers $
    setAuthSessionAge 360000 $
    defaultAuthSettings

authenticationMiddleware :: Config -> IO Middleware
authenticationMiddleware = mkAuthMiddleware . authSettings

--------------------------------------------------------------------------------
-- User Identity

data Token = Token
  { token_access :: AccessToken
  , token_type :: Maybe Text
  , token_refresh :: Maybe RefreshToken
  , token_expires :: Maybe Int
  , token_scope :: Scope
  , token_orcid :: Id
  , token_name :: Text
  } deriving Show

newtype Scope = Scope Text
  deriving newtype (Show, FromJSON)

newtype Id = Id Text
  deriving newtype (Show, FromJSON)

instance FromJSON Token where
  parseJSON = withObject "Token" $ \o ->
    Token <$>
      (o .: "access_token") <*>
      (o .:? "token_type") <*>
      (o .:? "refresh_token") <*>
      (o .:? "expires_in") <*>
      (o .: "scope") <*>
      (o .: "orcid") <*>
      (o .: "name")

getToken :: HasCallStack => Request -> Token
getToken request =
  case getAccessToken request of
    Nothing ->
      error "No user identity after authorization middleware."
    Just token ->
      token

--------------------------------------------------------------------------------
-- Get Record

getUserRecord :: Endpoint -> AccessToken -> Id -> IO (Either ByteString Value)
getUserRecord endpoint token (Id user) = do
  let
    url = makeAPIHost endpoint <> "/v3.0/" <> user <> "/record"
    uri =
     case parseURI strictURIParserOptions (encodeUtf8 url) of
       Left e -> error (show e)
       Right a -> a

  manager <- HTTP.TLS.newTlsManager
  authGetJSON manager token uri
