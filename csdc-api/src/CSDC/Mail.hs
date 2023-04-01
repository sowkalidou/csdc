{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module CSDC.Mail
  ( Config (..),
    Context (..),
    activate,
    Action (..),
    run,
    Mail (..),
    send,

    -- * Reexport
    Address (..),
  )
where

import CSDC.Prelude
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.Pool
import Data.Text.Lazy qualified as Text.Lazy
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP
import Network.Socket (HostName)

--------------------------------------------------------------------------------
-- Config

data Config = Config
  { hostName :: HostName,
    portNumber :: Int,
    userName :: UserName,
    password :: Password
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)

newtype Context = Context
  { pool :: Pool SMTPConnection
  }
  deriving (Show)

activate :: Config -> IO Context
activate Config {..} =
  let connect = do
        conn <- connectSMTPSTARTTLS' hostName (fromIntegral portNumber)
        _ <- login conn userName password
        pure conn
   in Context <$> createPool connect closeSMTP 1 10 10

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
  { from :: Address,
    to :: [Address],
    subject :: Text,
    text :: Text,
    html :: Text
  }
  deriving (Show, Eq)

send :: Mail -> Action ()
send Mail {..} = do
  let name = case addressName from of
        Nothing -> "CS-DC DAO"
        Just n -> n <> " via CS-DC DAO"

      mail =
        Mime.Mail
          { mailFrom = Address (Just name) "guaraqe@mailbox.org",
            mailTo = to,
            mailCc = [],
            mailBcc = [],
            mailHeaders =
              [ ("Subject", subject),
                ("Reply-To", Mime.renderAddress from)
              ],
            mailParts =
              [ [ Mime.plainPart (Text.Lazy.fromStrict text),
                  Mime.htmlPart (Text.Lazy.fromStrict html)
                ]
              ]
          }

  ask >>= \case
    Just (Context pool) ->
      liftIO $ withResource pool $ \connection ->
        renderAndSend connection mail
    Nothing -> liftIO $ do
      bs <- Mime.renderMail' mail
      ByteString.putStrLn $ "\n" <> bs <> "\n"
