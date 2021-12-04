module CSDC.UI.PreviewImageText exposing
  ( view
  )

import CSDC.UI.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Markdown

view : { value | name : String, description : String, image : String } -> msg -> Html msg
view value msg = Preview.make
  [ Html.div
     [ Html.Attributes.style "width" "100%"
     , Html.Attributes.style "min-height" "156px"
     ]
     [ Html.h4 [] [ Html.text value.name ]

     , Html.figure
         [ Html.Attributes.class "image is-96x96"
         , Html.Attributes.style "float" "right"
         , Html.Attributes.style "margin" "5px 0px 10px 30px"
         ]
         [ Html.img
             [ Html.Attributes.src value.image
             , Html.Attributes.style "border-radius" "10%"
             , Html.Attributes.alt "Profile photo"
             ]
             []
         ]

     , Markdown.toHtml
         [ Html.Attributes.class "content"]
         value.description
     ]

  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick msg
      ]
      [ Html.text "See profile"
      ]
  ]
