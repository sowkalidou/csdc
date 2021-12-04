module CSDC.View.UnitPreview exposing
  ( view
  )

import CSDC.UI.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : Unit -> msg -> Html msg
view unit viewUnit = Preview.make
  [ Html.p
      [ Html.Attributes.style "white-space" "pre-wrap"
      ]
      [ Html.strong [] [ Html.text unit.name ]
      , Html.br [] []
      , Html.text unit.description
      ]
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick viewUnit
      ]
      [ Html.text "View Unit"
      ]
  ]
