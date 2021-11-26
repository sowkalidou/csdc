module Form exposing (..)

import CSDC.Notification as Notification exposing (Notification, Has)
import CSDC.API as API
import Delay

type Msg msg b
  = ModelMsg msg
  | Submit
  | Response (API.Response b)
  | ResetNotification
  | Finish b

isFinished : Msg msg b -> Bool
isFinished msg =
  case msg of
    Finish _ -> True
    _ -> False

type alias Config model msg a b =
  { initial : Has model
  , update : msg -> Has model -> (Has model, Cmd msg)
  , reload : Has model -> Has model
  , parse : Has model -> Maybe a
  , request : a -> Cmd (API.Response b)
  , finish : b -> Cmd (Msg msg b)
  }

update :
  Config model msg a b ->
  Msg msg b -> Has model -> (Has model, Cmd (Msg msg b))
update config formMsg model =
  case formMsg of
    ModelMsg msg ->
      let
        (updated, cmd) = config.update msg model
      in
        (updated, Cmd.map ModelMsg cmd)

    Submit ->
      let
        reloaded = config.reload model
      in
        case config.parse reloaded of
          Nothing ->
            ( { model
              | notification = Notification.Error ["All fields should be filled"]
              }
            , Cmd.none
            )
          Just a ->
            ( { model | notification = Notification.Processing }
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

