{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CSDC.Daemon.Mail
  ( daemon
  ) where

import CSDC.Action
import CSDC.Daemon (Daemon)

import qualified CSDC.Daemon as Daemon
import qualified CSDC.Mail as Mail
import qualified CSDC.SQL.Mail as SQL.Mail

import Control.Monad (forM_, unless)
import UnliftIO

-- | A daemon that checks the e-mail queue and tries to send them. If
-- successful, remove them from the queue.
daemon :: Daemon (Action user)
daemon = Daemon.make 5 "Mail" $ do
  successesRef <- liftIO $ newIORef (0 :: Int)
  failuresRef <- liftIO $ newIORef (0 :: Int)

  tuples <- runQuery SQL.Mail.select ()
  forM_ tuples $ \(mailId, mail) ->
    UnliftIO.try (runMail (Mail.send mail)) >>= \case
      Left (err :: SomeException) -> do
        liftIO $ putStrLn $ displayException err
        modifyIORef' failuresRef (+1)
      Right () -> do
        runQuery SQL.Mail.delete mailId
        modifyIORef' successesRef (+1)

  successes <- liftIO $ readIORef successesRef
  failures <- liftIO $ readIORef failuresRef

  unless (successes + failures == 0) $
    liftIO $ putStrLn $
    "Mail: OK - " <> show successes <> ", FAIL - " <> show failures <> "."
