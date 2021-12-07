module UI.Column exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes

view : String -> List (Html msg) -> List (Html msg) -> Html msg
view title right children =
  Html.div
    [ Html.Attributes.class "box"
    , Html.Attributes.id (title ++ "-panel")
    , Html.Attributes.style "height" "100%"
    , Html.Attributes.style "overflow-y" "hidden"
    , Html.Attributes.style "padding-bottom" "85px"
    ]
    [ Html.div
        [ Html.Attributes.class
            "is-flex is-flex-direction-row is-justify-content-space-between"
        , Html.Attributes.style "padding" "5px"
        ] <|
        [ viewTitle title
        ] ++ right
    , Html.div
        [ Html.Attributes.id (title ++ "-items")
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "padding" "5px"
        , Html.Attributes.style "overflow-y" "auto"
        , Html.Attributes.style "scroll-behavior" "smooth"
        ]
        children
    ]

viewTitle : String -> Html msg
viewTitle name =
  Html.h4
    [ Html.Attributes.class "title"
    ]
    [ Html.text name
    ]
