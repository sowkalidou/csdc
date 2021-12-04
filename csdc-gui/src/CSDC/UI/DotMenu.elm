module CSDC.UI.DotMenu exposing
  ( Item
  , make
  )

import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Item msg =
  { label: String
  , message: msg
  }

make : List (Item msg) -> Html msg
make items =
  Html.div
    [ Html.Attributes.class "dropdown is-hoverable is-right"
    , Html.Attributes.style "height" "26px"
    , Html.Attributes.style "margin-top" "-4px"
    ]
    [ Html.div
        [ Html.Attributes.class "dropdown-trigger"
        ]
        [ Html.button
            [ Html.Attributes.class "button is-small is-rounded"
            , Html.Attributes.style "width" "30px"
            , Html.Attributes.attribute "aria-haspopup" "true"
            , Html.Attributes.attribute "aria-controls" "dropdown-menu"
            ]
            [ Html.span
              [ Html.Attributes.style "font-size" "16px"
              , Html.Attributes.style "padding-left" "2px"
              ]
              [ Html.text "\u{2807}" ]
            ]
        ]
    , Html.div
        [ Html.Attributes.class "dropdown-menu"
        , Html.Attributes.attribute "role" "menu"
    , Html.Attributes.style "min-width" "220px"
        ]
        [ Html.div
            [ Html.Attributes.class "dropdown-content has-text-right"
            ]
            (List.map makeItem items)
        ]
    ]

makeItem : Item msg -> Html msg
makeItem { label, message } =
  Html.div
    [ Html.Attributes.class "dropdown-item"
    , Html.Events.onClick message
    ]
    [ Html.text label
    ]
