{-# LANGUAGE RecordWildCards #-}

module CSDC.Mail
  ( Context (..)
  , Action (..)
  , run
  , Mail (..)
  , send
  ) where

import CSDC.Prelude

import Control.Exception
import Control.Monad.Reader
import Network.Mail.Mime hiding (Mail, simpleMail)
import Network.Mail.SMTP
import Network.Socket (HostName)

import qualified Data.Text.Lazy as Text.Lazy

--------------------------------------------------------------------------------
-- Context

data Context = Context
  { context_hostName :: HostName
  , context_portNumber :: Int
  , context_userName :: UserName
  , context_password :: Password
  } deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via JSON Context

withSMTPConnection :: Context -> (SMTPConnection -> IO a) -> IO a
withSMTPConnection Context {..} action =
  let
    connect = do
      conn <- connectSMTPS' context_hostName (fromIntegral context_portNumber)
      _ <- login conn context_userName context_password
      pure conn
  in
    bracket connect closeSMTP action

--------------------------------------------------------------------------------
-- Action

newtype Action a = Action (ReaderT SMTPConnection IO a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader SMTPConnection, MonadIO)

run :: MonadIO m => Context -> Action a -> m a
run context (Action action) = liftIO $
  withSMTPConnection context (runReaderT action)

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
  let parts = [plainPart (Text.Lazy.fromStrict text)]
      mail = simpleMail from to [] [] subject parts
  connection <- ask
  liftIO $ renderAndSend connection mail
