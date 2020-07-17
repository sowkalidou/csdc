module CSDC.Component.ReplyMember exposing
  ( Param
  , Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)
import Field exposing (Field)
import Input
import Validation exposing (Validation)

import Element exposing (..)
import Element.Font as Font
import Element.Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Param =
  { person : Id Person
  , message : Id (Message Member)
  , messageType : MessageType
  }

type alias Model =
  { text : String
  , replyType : Field (Maybe ReplyType) ReplyType
  , notification : Notification
  }

initial : Model
initial =
  { text = ""
  , replyType = Field.required "Reply Type"
  , notification = Notification.Empty
  }

validate : Param -> Model -> Result (List String) (Reply Member)
validate param model =
  let
    result =
      Validation.valid makeReply
        |> Validation.andMap (Field.validate model.replyType)
        |> Validation.andMap (Validation.valid param.messageType)
        |> Validation.andMap (Validation.valid model.text)
        |> Validation.andMap (Validation.valid NotSeen)
        |> Validation.andMap (Validation.valid param.message)
  in
    Validation.validate result

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputText String
  | APIMsg API.Msg
  | InputReplyType ReplyType
  | Submit
  | Reset

update : Msg -> Param -> Model -> (Model, Cmd Msg)
update msg param model =
  case msg of
    InputText str ->
      ( { model | text = str }
      , Cmd.none
      )

    Submit ->
      case validate param model of
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

    APIMsg apimsg ->
      case apimsg of
        API.InsertMember result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok _ ->
              ( { initial | notification = Notification.Success }
              , Notification.reset Reset
              )

        API.SendReplyMember result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok _ ->
              ( { initial | notification = Notification.Success }
              , Notification.reset Reset
              )

        _ ->
          (model, Cmd.none)

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Param -> Model -> Element Msg
view param model =
  column [ width <| fillPortion 2, padding 10, spacing 10 ] <|
    [ row
        [ Font.bold, Font.size 30 ]
        [ case param.messageType of
            Submission ->
              text <| "Submission to the unit"
            Invitation ->
              text <| "Invitation of the unit"
        ]
    , selectReplyType model
    , Element.Input.multiline []
        { label = Element.Input.labelAbove [] (text "Your message.")
        , onChange = InputText
        , placeholder = Nothing
        , text = model.text
        , spellcheck = True
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification

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
