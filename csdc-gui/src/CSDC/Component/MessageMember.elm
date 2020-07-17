module CSDC.Component.MessageMember exposing
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
import Validation exposing (Validation)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Param =
  { person : WithId Person
  , unit : WithId Unit
  , messageType : MessageType
  }

type alias Model =
  { text : String
  , notification : Notification
  }

initial : Model
initial =
  { text = ""
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputText String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Msg -> Param -> Model -> (Model, Cmd Msg)
update msg param model =
  case msg of
    InputText text ->
      ( { model | text = text }
      , Cmd.none
      )

    Submit ->
      let
        message = Message
         { mtype = param.messageType
         , text = model.text
         , status = Waiting
         , value = makeMember param.person.id param.unit.id
         }
      in
        ( { model | notification = Notification.Processing }
        , Cmd.map APIMsg <| API.sendMessageMember message
        )

    APIMsg apimsg ->
      case apimsg of
        API.SendMessageMember result ->
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
              text <| "Submission for " ++ param.unit.value.name
            Invitation ->
              text <| "Invitation for " ++ param.person.value.name
        ]
    , Input.multiline []
        { label = Input.labelAbove [] (text "Your message.")
        , onChange = InputText
        , placeholder = Nothing
        , text = model.text
        , spellcheck = True
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification

