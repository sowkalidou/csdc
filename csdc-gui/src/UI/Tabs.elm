module UI.Tabs exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode

--------------------------------------------------------------------------------
-- Model

view : (tab -> Bool) -> List (tab, String) -> Html tab
view isSelected tabs =
  Html.div
    [ Html.Attributes.class "tabs is-toggle"
    , Html.Attributes.style "margin-top" "-40px"
    ]
    [ Html.ul
        []
        (List.map (viewTab isSelected) tabs)
    ]

viewTab : (tab -> Bool) -> (tab, String) -> Html tab
viewTab isSelected (tab, name) =
  Html.li
    ( if isSelected tab
      then [ Html.Attributes.class "is-active" ]
      else []
    )
    [ Html.a
        [ Html.Attributes.target "_self"
        , Html.Events.preventDefaultOn "click" <| Decode.succeed (tab, True)
        ]
        [ Html.text name ]
    ]
