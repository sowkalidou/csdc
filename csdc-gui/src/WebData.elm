module WebData exposing (..)

import Notification exposing (Notification)

import Html exposing (Html)
import Html.Attributes
import Http

type WebData a = Loading | Error Http.Error | Success a

view : WebData a -> (a -> List (Html msg)) -> List (Html msg)
view data f =
  case data of
    Loading ->
      [ Html.div
          [ Html.Attributes.class "columns is-desktop is-vcentered"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column" ]
              [ Html.progress
                  [ Html.Attributes.class "progress is-small is-info"
                  , Html.Attributes.attribute "max" "100"
                  ]
                  []
              ]
          ]
      ]

    Error _ ->
      [ Html.article
          [ Html.Attributes.class "message is-danger" ]
          [ Html.div
              [ Html.Attributes.class "message-header" ]
              [ Html.text "Error" ]
          , Html.div
              [ Html.Attributes.class "message-body" ]
              [ Html.text "An HTTP error occurred." ]
          ]
      ]

    Success a -> f a

update : model -> WebData a -> (a -> (model, Cmd msg)) -> (model, Cmd msg)
update model data f =
  case data of
    Success a -> f a
    _ -> (model, Cmd.none)
