{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module CSDC.Mail.Templates where

import CSDC.Prelude
import CSDC.Mail
import CSDC.Mail.Templates.TH

import Data.Aeson
import Data.Aeson.Types
import Text.Blaze.Html.Renderer.Text
import Text.Mustache
import Text.Markdown

import qualified Data.Text.Lazy as Text.Lazy

--------------------------------------------------------------------------------
-- Templates generator

render :: Template -> [Pair] -> (Text, Text)
render template value =
  let
    md = renderMustache template (object value)

    plain = Text.Lazy.toStrict md

    htmlBody = Text.Lazy.toStrict $ renderHtml $ markdown def md

    html = "<html><body>" <> htmlBody <> "</body></html>"
  in
    (plain, html)

--------------------------------------------------------------------------------
-- Templates

confirmation :: NewUser -> Mail
confirmation NewUser {..} =
  let
    template = $(templateFor "confirmation")
    (text, html) = render template
      [ "email" .= newUser_email
      ]
  in
    Mail
      { from = Address Nothing "mail@guaraqe.gq"
      , to = [Address (Just newUser_name) newUser_email]
      , subject = "Your account has been created successfully"
      , text = text
      , html = html
      }

invitation :: Unit -> Person -> Text -> Text -> Mail
invitation fromUnit fromUnitChair message toPerson =
  let
    template = $(templateFor "invitation")
    (text, html) = render template
      [ "chairName" .= person_name fromUnitChair
      , "unitName" .= unit_name fromUnit
      , "inviteeEmail" .= toPerson
      , "message" .= message
      ]
  in
    Mail
      { from = personAddress fromUnitChair
      , to = [Address Nothing toPerson]
      , subject = "Invitation to join " <> unit_name fromUnit <> "."
      , text = text
      , html = html
      }

--------------------------------------------------------------------------------
-- Helpers

personAddress :: Person -> Address
personAddress Person {..} = Address (Just person_name) person_email
