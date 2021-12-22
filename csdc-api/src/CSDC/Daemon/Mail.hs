{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CSDC.Daemon.Mail
  ( daemon
  ) where

import CSDC.Action
import CSDC.Daemon (Daemon)
import Control.Monad (forM_)

import qualified CSDC.Daemon as Daemon
import qualified CSDC.Mail as Mail
import qualified CSDC.SQL.Mail as SQL.Mail

-- | A daemon that checks the e-mail queue and tries to send them. If
-- successful, remove them from the queue.
daemon :: Daemon (Action user)
daemon = Daemon.make 5 "Mail" $ do
  -- XXX: check for errors
  tuples <- runQuery SQL.Mail.select ()
  forM_ tuples $ \(mailId, mail) -> do
    runMail (Mail.send mail)
    runQuery SQL.Mail.delete mailId
