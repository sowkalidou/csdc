module CSDC.Component.PreviewPerson exposing
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

view : Person -> msg -> Html msg
view person viewPerson =
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
            ]
        ]
    ]
