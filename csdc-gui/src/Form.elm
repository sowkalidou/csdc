module Form exposing
  ( Msg (..)
  , isFinished
  , update
  , Config
  , StatelessConfig
  , statelessConfig
  , viewWith
  )

import UI.Preview as Preview
import Notification as Notification exposing (Has)
import API as API
import Html exposing (Html)
import Delay

type Msg msg r b
  = ModelMsg msg
  | Submit r
  | Response (API.Response b)
  | ResetNotification
  | Finish b

isFinished : Msg msg r b -> Bool
isFinished msg =
  case msg of
    Finish _ -> True
    _ -> False

type alias Config model msg r a b =
  { initial : Has model
  , update : msg -> Has model -> (Has model, Cmd msg)
  , reload : Has model -> Has model
  , parse : r -> Has model -> Maybe a
  , request : a -> Cmd (API.Response b)
  , finish : b -> Cmd (Msg msg r b)
  }

type alias StatelessConfig b =
  { request : Cmd (API.Response b)
  , finish : b -> Cmd (Msg () () b)
  }

statelessConfig : Has model -> StatelessConfig b -> Config model () () () b
statelessConfig initial config =
  { initial = initial
  , update = \_ model -> (model, Cmd.none)
  , reload = \model -> model
  , parse = \_ _ -> Just ()
  , request = \_ -> config.request
  , finish = config.finish
  }

update :
  Config model msg r a b ->
  Msg msg r b -> Has model -> (Has model, Cmd (Msg msg r b))
update config formMsg model =
  case formMsg of
    ModelMsg msg ->
      let
        (updated, cmd) = config.update msg model
      in
        (updated, Cmd.map ModelMsg cmd)

    Submit r ->
      let
        reloaded = config.reload model
      in
        case config.parse r reloaded of
          Nothing ->
            ( { reloaded
              | notification = Notification.Error ["All fields should be filled"]
              }
            , Delay.after 3 Delay.Second ResetNotification
            )
          Just a ->
            ( { reloaded
              | notification = Notification.Processing
              }
            , Cmd.map Response <| config.request a
            )

    Response response ->
      Notification.withResponse ResetNotification model response <| \b ->
        ( { model | notification = Notification.Success }
        , Delay.after 1 Delay.Second <| Finish b
        )

    Finish b ->
      ( config.initial, config.finish b )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

viewWith : String -> (Has model -> List (Html msg)) -> Has model -> Html msg
viewWith title view model =
  Preview.make <|
  [ Html.h2 [] [ Html.text title ] ] ++
  view model ++
  Notification.view model.notification
