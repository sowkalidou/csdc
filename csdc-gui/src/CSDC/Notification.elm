module CSDC.Notification exposing
  ( Notification (..)
  , view
  , reset
  , withResponse
  )

import Delay
import Element exposing (..)
import Http

type Notification
  = Empty
  | Processing
  | Success
  | Error (List String)
  | HttpError Http.Error

view : Notification -> List (Element msg)
view notification =
  case notification of
    Empty ->
      []

    Processing ->
      [ text "Processing..." ]

    Success ->
      [ text "Success!" ]

    Error msgs ->
      [ text "Error: " ] ++ List.map text msgs

    HttpError err ->
      case err of
        Http.BadUrl msg ->
          [ text <| "Bad url: " ++ msg ]

        Http.Timeout ->
          [ text "Timeout." ]

        Http.NetworkError ->
          [ text "Network error." ]

        Http.BadStatus n ->
          [ text <| "Bad status: " ++ String.fromInt n ]

        Http.BadBody msg ->
          [ text <| "Bad body: " ++ msg ]

reset : msg -> Cmd msg
reset = Delay.after 2 Delay.Second

type alias Has model = { model | notification : Notification }

withResponse :
  msg ->
  Has model ->
  Result Http.Error a ->
  (a -> (Has model, Cmd msg)) ->
  (Has model, Cmd msg)
withResponse resetMsg model result onSuccess =
  case result of
    Err err ->
      ( { model | notification = HttpError err }
      , reset resetMsg
      )
    Ok a ->
      onSuccess a
