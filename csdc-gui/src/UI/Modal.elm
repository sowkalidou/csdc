module UI.Modal exposing
  ( view
  , viewMaybe
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

viewMaybe : Maybe a -> msg -> (a -> Html msg) -> Html msg
viewMaybe mval close makeContents =
  let
    isActive = case mval of
      Nothing -> False
      Just _ -> True
  in
    view isActive close <|
      case mval of
        Nothing -> Html.div [] []
        Just val -> makeContents val
