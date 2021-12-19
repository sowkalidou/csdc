{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module CSDC.API.Auth
  ( API
  , serveAPI
  , Settings (..)
  , makeSettings
  , makeContext
  , contextProxy
  ) where

import CSDC.Action hiding (Context)
import CSDC.DAO (createUser)
import CSDC.Prelude hiding (JSON, Post)

import qualified CSDC.API.DAO as DAO
import qualified CSDC.SQL.Persons as SQL.Persons

import Data.Password.Bcrypt (PasswordCheck (..), mkPassword, checkPassword)
import Servant hiding (Server)
import Servant.Auth.Server

--------------------------------------------------------------------------------
-- User

newtype User = User { getUser :: Id Person }
   deriving (Eq, Show, Generic)
   deriving newtype (FromJSON, ToJSON)

instance ToJWT User
instance FromJWT User

--------------------------------------------------------------------------------
-- Login

data Login = Login { email :: !Text, password :: !Text}
   deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

--------------------------------------------------------------------------------
-- Signin API

type CookieHeaders =
  Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]

type SigninAPI =
  ReqBody '[JSON] Login :> Verb 'POST 204 '[JSON] (CookieHeaders NoContent)

serveSigninAPI :: Settings -> Server SigninAPI
serveSigninAPI settings = authenticate settings

authenticate :: Settings -> Login -> Action () (CookieHeaders NoContent)
authenticate (Settings cookieSettings jwtSettings) (Login email password) =
   runQuery SQL.Persons.check email >>= \case
     Nothing -> error "401"
     Just (personId, passwordHash) -> do
       case checkPassword (mkPassword password) passwordHash of
         PasswordCheckFail ->
           error "401"
         PasswordCheckSuccess -> do
           mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (User personId)
           case mApplyCookies of
             Nothing           -> error "AAA" -- throwError err401
             Just applyCookies -> return $ applyCookies NoContent

--------------------------------------------------------------------------------
-- Signup API

type SignupAPI =
  ReqBody '[JSON] NewUser :> Post '[JSON] (Id Person)

serveSignupAPI :: Server SignupAPI
serveSignupAPI = createUser

--------------------------------------------------------------------------------
-- Signout API

type SignoutAPI =
  Get '[JSON] (CookieHeaders Text)

serveSignoutAPI :: Monad m => Settings -> ServerT SignoutAPI m
serveSignoutAPI (Settings {..}) = return $
  clearSession settingsCookie ""

--------------------------------------------------------------------------------
-- API with auth

type AuthAPI = Auth '[Cookie] User :> DAO.API

serveAuthAPI :: AuthResult User -> Server DAO.API
serveAuthAPI (Authenticated (User personId)) =
  hoistServer (Proxy @DAO.API) (withPerson personId) DAO.serveAPI
serveAuthAPI res = error $ show res

type API =
  "api" :> AuthAPI :<|>
  "signin" :> SigninAPI :<|>
  "signup" :> SignupAPI :<|>
  "signout" :> SignoutAPI

data Settings = Settings
  { settingsCookie :: CookieSettings
  , settingsJWT :: JWTSettings
  }

makeSettings :: IO Settings
makeSettings = do
  key <- generateKey
  pure Settings
    { settingsCookie = defaultCookieSettings { cookieXsrfSetting = Nothing }
    , settingsJWT = defaultJWTSettings key
    }

serveAPI :: Settings -> Server API
serveAPI settings =
  serveAuthAPI :<|>
  serveSigninAPI settings :<|>
  serveSignupAPI :<|>
  serveSignoutAPI settings

contextProxy :: Proxy '[CookieSettings, JWTSettings]
contextProxy = Proxy

makeContext :: Settings -> Context '[CookieSettings, JWTSettings]
makeContext Settings {..} =
  settingsCookie :. settingsJWT :. EmptyContext
