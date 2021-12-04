module CSDC.UI.Tabs exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode

--------------------------------------------------------------------------------
-- Model

view : tab -> List (tab, String) -> Html tab
view selected tabs =
  Html.div
    [ Html.Attributes.class "tabs is-toggle"
    , Html.Attributes.style "margin-top" "-40px"
    ]
    [ Html.ul
        []
        (List.map (viewTab selected) tabs)
    ]

viewTab : tab -> (tab, String) -> Html tab
viewTab selected (tab, name) =
  Html.li
    ( if selected == tab
      then [ Html.Attributes.class "is-active" ]
      else []
    )
    [ Html.a
        [ Html.Attributes.target "_self"
        , Html.Events.preventDefaultOn "click" <| Decode.succeed (tab, True)
        ]
        [ Html.text name ]
    ]
