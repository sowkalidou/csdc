module Notification exposing
  ( Notification (..)
  , with
  , view
  , reset
  , withResponse
  , Has
  )

import Delay
import Html exposing (Html)
import Html.Attributes
import Http
import Page

type Notification
  = Empty
  | Processing
  | Success
  | Error (List String)
  | HttpError Http.Error

with : Notification -> List (Html msg) -> List (Html msg)
with notification html = html ++ view notification

view : Notification -> List (Html msg)
view notification =
  let
    body = case notification of
      Empty ->
        []

      Processing ->
        [ Html.text "Processing..." ]

      Success ->
        [ Html.text "Success!" ]

      Error msgs ->
        List.map (\t -> Html.div [] [ Html.text t ]) msgs

      HttpError err ->
        case err of
          Http.BadUrl msg ->
            [ Html.text <| "Bad url: " ++ msg ]

          Http.Timeout ->
            [ Html.text "Timeout." ]

          Http.NetworkError ->
            [ Html.text "Network error." ]

          Http.BadStatus n ->
            [ Html.text <| "Bad status: " ++ String.fromInt n ]

          Http.BadBody msg ->
            [ Html.text <| "Bad body: " ++ msg ]
  in
    case notification of
      Empty ->
        []
      _ ->
        [ Html.div
            [ case notification of
                Success ->
                  Html.Attributes.class "notification is-success is-light"
                Processing ->
                  Html.Attributes.class "notification is-info is-light"
                _ ->
                  Html.Attributes.class "notification is-danger is-light"
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "z-index" "1"
            , Html.Attributes.style "bottom" "20px"
            , Html.Attributes.style "width" "600px"
            , Html.Attributes.style "left" "50%"
            , Html.Attributes.style "transform" "translate(-50%, 0)"
            ]
            body
        ]

reset : msg -> Cmd msg
reset = Delay.after 3 Delay.Second

type alias Has model = { model | notification : Notification }

withResponse :
  Page.Info ->
  msg ->
  Has model ->
  Result Http.Error a ->
  (a -> (Has model, Cmd msg)) ->
  (Has model, Cmd msg)
withResponse pageInfo resetMsg model result onSuccess =
  case result of
    Err err ->
      case err of
        Http.BadStatus 401 ->
          ( { model | notification = Error ["Authentication error."] }
          , Cmd.batch
              [ Page.goTo pageInfo Page.SignIn
              , reset resetMsg
              ]
          )
        _ ->
          ( { model | notification = HttpError err }
          , reset resetMsg
          )
    Ok a -> onSuccess a
