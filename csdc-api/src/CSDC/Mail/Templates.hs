{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module CSDC.Mail.Templates where

import CSDC.Mail
import CSDC.Mail.Templates.TH
import CSDC.Prelude
import Data.Aeson
import Data.Aeson.Types
import Data.Text.Lazy qualified as Text.Lazy
import Text.Blaze.Html.Renderer.Text
import Text.Markdown
import Text.Mustache

--------------------------------------------------------------------------------
-- Templates generator

render :: Template -> [Pair] -> (Text, Text)
render template value =
  let md = renderMustache template (object value)

      plain = Text.Lazy.toStrict md

      htmlBody = Text.Lazy.toStrict $ renderHtml $ markdown def md

      html = "<html><body>" <> htmlBody <> "</body></html>"
   in (plain, html)

--------------------------------------------------------------------------------
-- Templates

confirmation :: NewUser -> Mail
confirmation NewUser {..} =
  let template = $(templateFor "confirmation")
      (text, html) =
        render
          template
          [ "email" .= email
          ]
   in Mail
        { from = Address Nothing "mail@guaraqe.gq",
          to = [Address (Just name) email],
          subject = "Your account has been created successfully",
          text = text,
          html = html
        }

invitation :: Unit -> Person -> Text -> Text -> Mail
invitation fromUnit fromUnitChair message toPerson =
  let template = $(templateFor "invitation")
      (text, html) =
        render
          template
          [ "chairName" .= fromUnitChair.name,
            "unitName" .= fromUnit.name,
            "inviteeEmail" .= toPerson,
            "message" .= message
          ]
   in Mail
        { from = personAddress fromUnitChair,
          to = [Address Nothing toPerson],
          subject = "Invitation to join " <> fromUnit.name <> ".",
          text = text,
          html = html
        }

--------------------------------------------------------------------------------
-- Helpers

personAddress :: Person -> Address
personAddress Person {..} = Address (Just name) email
