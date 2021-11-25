module CSDC.Component.Modal exposing
  ( view
  )

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : Bool -> msg -> Html msg -> Html msg
view isActive close contents =
  Html.div
    [ if isActive
      then Html.Attributes.class "modal is-active"
      else Html.Attributes.class "modal"
    ]
    [ Html.div
        [ Html.Attributes.class "modal-background"
        , Html.Events.onClick close
        ]
        []
    , Html.div
        [ Html.Attributes.class "modal-content"
        ]
        [ contents ]
    , Html.button
        [ Html.Attributes.class "modal-close is-large"
        , Html.Attributes.attribute "aria-label" "close"
        , Html.Events.onClick close
        ]
        []
    ]
