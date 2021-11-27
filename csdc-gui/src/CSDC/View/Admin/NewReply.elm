module CSDC.View.Admin.NewReply exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)
import Field exposing (Field)
import Input
import Validation exposing (Validation)

import Element exposing (..)
import Element.Font as Font
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { message : Field String (Id (Message Member))
  , replyType : Field (Maybe ReplyType) ReplyType
  , notification : Notification
  }

initial : Model
initial =
  { message = Field.requiredId "Message"
  , replyType = Field.required "Reply Type"
  , notification = Notification.Empty
  }

validate : Model -> Result (List String) (NewReply Member)
validate model =
  let
    result =
      Validation.valid NewReply
        |> Validation.andMap (Field.validate model.replyType)
        |> Validation.andMap (Validation.valid "Reply")
        |> Validation.andMap (Field.validate model.message)
  in
    Validation.validate result

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputMessage String
  | APIMsg (API.Response ())
  | InputReplyType ReplyType
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputMessage str ->
      ( { model | message = Field.set str model.message }
      , Cmd.none
      )

    Submit ->
      case validate model of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok reply ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.sendReplyMember reply
          )

    InputReplyType replyType ->
      ( { model | replyType = Field.set (Just replyType) model.replyType }
      , Cmd.none
      )

    APIMsg result ->
      case result of
        Err err ->
          ( { model | notification = Notification.HttpError err }
          , Cmd.none
          )
        Ok _ ->
          ( { initial | notification = Notification.Success }
          , Notification.reset Reset
          )

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> Element Msg
view model =
  column [ width <| fillPortion 2, padding 10, spacing 10 ] <|
    [ row
        [ Font.bold, Font.size 30 ]
        [ text "New Reply" ]
    , selectReplyType model
    , Input.text
        { onChange = InputMessage
        , field = model.message
        }
    , Input.button Submit "Submit"
    ] ++ List.map html (Notification.view model.notification)

selectReplyType : Model -> Element Msg
selectReplyType model =
  Input.radio
    { onChange = InputReplyType
    , field = model.replyType
    , options =
        [ (Accept, "Accept")
        , (Reject, "Reject")
        ]
    }
