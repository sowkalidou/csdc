module CSDC.Component.Preview exposing
  ( make
  )

import CSDC.Types exposing (..)
import CSDC.Input exposing (button)

import Html exposing (Html)
import Html.Attributes
import Html.Events

make : List (Html msg) -> Html msg
make children =
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
                children
            ]
        ]
    ]
