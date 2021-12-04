module CSDC.UI.Progress exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes

view : Html msg
view =
  Html.div
    [ Html.Attributes.class "columns is-desktop is-vcentered"
    , Html.Attributes.style "height" "100%"
    ]
    [ Html.div
        [ Html.Attributes.class "column is-one-third" ]
        []
    , Html.div
        [ Html.Attributes.class "column is-one-third" ]
        [ Html.progress
            [ Html.Attributes.class "progress is-small is-info"
            , Html.Attributes.attribute "max" "100"
            ]
            []
        ]
    , Html.div
        [ Html.Attributes.class "column is-one-third" ]
        []
    ]
