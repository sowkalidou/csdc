{-# LANGUAGE RecordWildCards #-}

module CSDC.Mail.Templates where

import CSDC.Prelude
import CSDC.Mail

import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Templates

confirmation :: NewUser -> Mail
confirmation NewUser {..} = Mail
  { from = Address Nothing "no-reply@csdc.org"
  , to = [Address (Just newUser_name) newUser_email]
  , subject = "Your account has been created successfully"
  , text = "Now you can login with the e-mail " <> newUser_email <> "."
  }

invitation :: Unit -> Person -> Text -> Text -> Mail
invitation fromUnit fromUnitChair message toPerson = Mail
  { from = personAddress fromUnitChair
  , to = [Address Nothing toPerson]
  , subject = "Invitation to join " <> unit_name fromUnit <> "."
  , text = Text.unlines
      [ "Hello,"
      , ""
      , "You have been invited by " <> person_name fromUnitChair <> " to join " <>
        unit_name fromUnit <> " at the CS-DC DAO:"
      , ""
      , "----------------------------------------"
      , ""
      , message
      , ""
      , "----------------------------------------"
      , ""
      , ""
      , "In order to join, please sign-up at the CS-DC DAO page:"
      , ""
      , "    https://csdc-dao-test.herokuapp.com"
      , ""
      , "using this e-mail address: " <> toPerson <> ". " <>
        "By doing so, you will be granted an account and be added to the unit."
      , ""
      , "If you have questions, reply to this e-mail and it will be sent to the unit chair, " <>
         person_name fromUnitChair <> "."
      , ""
      , "Best regards,"
      , ""
      , "CS-DC DAO Team"
      ]
  }

--------------------------------------------------------------------------------
-- Helpers

personAddress :: Person -> Address
personAddress Person {..} = Address (Just person_name) person_email
