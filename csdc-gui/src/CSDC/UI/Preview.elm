module CSDC.UI.Preview exposing
  ( make
  )

import CSDC.Types exposing (..)

import Html exposing (Html)
import Html.Attributes

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
