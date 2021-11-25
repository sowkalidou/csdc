module CSDC.View.PersonPreview exposing
  ( view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : Person -> msg -> Html msg
view person viewPerson = Preview.make
  [ Html.p
      []
      [ Html.strong [] [ Html.text person.name ]
      , Html.br [] []
      , Html.text person.description
      ]
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick viewPerson
      ]
      [ Html.text "View Person"
      ]
  ]
