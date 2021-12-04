module CSDC.UI.BoxPost exposing
  ( view
  )

import CSDC.Types exposing (PostInfo)

import Html exposing (Html)
import Html.Attributes
import Markdown

view : PostInfo -> Html msg
view post =
  Html.div
    [ Html.Attributes.class "box option-box has-background-white-ter is-shadowless"
    , Html.Attributes.style "margin-bottom" "1.0em"
    ]
    [ Html.strong [] [ Html.text post.authorName ]
    , Markdown.toHtml [] post.text
    ]
