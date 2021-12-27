{-# LANGUAGE RecordWildCards #-}

module CSDC.Mail
  ( Config (..)
  , Context (..)
  , activate
  , Action (..)
  , run
  , Mail (..)
  , send
    -- * Reexport
  , Address (..)
  ) where

import CSDC.Prelude

import Control.Monad.Reader
import Data.Pool
import Network.Mail.SMTP
import Network.Socket (HostName)

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text.Lazy as Text.Lazy
import qualified Network.Mail.Mime as Mime

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { config_hostName :: HostName
  , config_portNumber :: Int
  , config_userName :: UserName
  , config_password :: Password
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Config

newtype Context = Context
  { context_pool :: Pool SMTPConnection
  } deriving (Show)

activate :: Config -> IO Context
activate Config {..} =
  let
    connect = do
      conn <- connectSMTPS' config_hostName (fromIntegral config_portNumber)
      _ <- login conn config_userName config_password
      pure conn
  in
    Context <$> createPool connect closeSMTP 1 10 10

--------------------------------------------------------------------------------
-- Action

newtype Action a = Action (ReaderT (Maybe Context) IO a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader (Maybe Context), MonadIO)

run :: MonadIO m => Maybe Context -> Action a -> m a
run context (Action action) = liftIO $ runReaderT action context

--------------------------------------------------------------------------------
-- Mail

data Mail = Mail
  { from :: Address
  , to :: [Address]
  , subject :: Text
  , text :: Text
  } deriving (Show, Eq)

send :: Mail -> Action ()
send Mail {..} = do
  let
    name = case addressName from of
      Nothing -> "CS-DC DAO"
      Just n -> n <> "via CS-DC DAO"

    mail = Mime.Mail
      { mailFrom = Address (Just name) "no-reply@csdc.org"
      , mailTo = to
      , mailCc = []
      , mailBcc  = []
      , mailHeaders =
          [ ("Subject", subject)
          , ("Reply-To", Mime.renderAddress from)
          ]
      , mailParts =
          [ [ Mime.plainPart (Text.Lazy.fromStrict text) ]
          ]
      }

  ask >>= \case
    Just (Context pool) ->
      liftIO $ withResource pool $ \connection ->
        renderAndSend connection mail
    Nothing -> liftIO $ do
      bs <- Mime.renderMail' mail
      ByteString.putStrLn $ "\n" <> bs <> "\n"
