module CSDC.Component.PreviewUnit exposing
  ( view
  )

import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border

view : Unit -> msg -> Html msg
view unit viewUnit =
  Html.div
    [ Html.Attributes.class "box"
    ]
    [ Html.article
        [ Html.Attributes.class "media"
        ]
        [ Html.div
            [ Html.Attributes.class "media-content"
            ]
            [ Html.div
                [ Html.Attributes.class "content"
                ]
                [ Html.p
                    []
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
            ]
        ]
    ]
