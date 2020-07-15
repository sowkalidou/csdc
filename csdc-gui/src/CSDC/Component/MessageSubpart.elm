module CSDC.Component.MessageSubpart exposing
  ( Param
  , Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input exposing (button)
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
  { personInfo : PersonInfo
  , unit : WithId Unit
  , messageType : MessageType
  }

type alias Model =
  { text : String
  , unit : Maybe (Id Unit)
  , notification : Notification
  }

initial : Model
initial =
  { text = ""
  , unit = Nothing
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputText String
  | SelectInvitation (Id Unit)
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

    SelectInvitation id ->
      ( { model | unit = Just id }
      , Cmd.none
      )

    Submit ->
      case model.unit of
        Nothing ->
          ( { model | notification = Notification.Error ["Please select a unit."] }
          , Cmd.none
          )

        Just unit ->
          let
            message = Message
             { mtype = param.messageType
             , text = model.text
             , status = Waiting
             , value =
                 case param.messageType of
                   Invitation ->
                     Subpart unit param.unit.id
                   Submission ->
                     Subpart param.unit.id unit
             }
          in
            ( { model | notification = Notification.Processing }
            , Cmd.map APIMsg <| API.sendMessageSubpart message
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
              text <| "Invitation of " ++ param.unit.value.name
        ]
    , let
        units = personInfoChair param.personInfo
      in
        if List.isEmpty units
        then text "You must be the chair of a unit to send messages to units."
        else invitation model.unit units SelectInvitation

    , Input.multiline []
        { label = Input.labelAbove [] (text "Your message.")
        , onChange = InputText
        , placeholder = Nothing
        , text = model.text
        , spellcheck = True
        }
    , CSDC.Input.button Submit "Submit"
    ] ++ Notification.view model.notification

invitation :
  Maybe (Id a) ->
  List (Id a, { a | name : String }) ->
  (Id a -> msg) ->
  Element msg
invitation selected units makeEvent =
  let
    makeOption (id, unit) = Input.option id (text unit.name)
  in
    Input.radioRow
      [ padding 10
      , spacing 20
      ]
      { onChange = makeEvent
      , selected = selected
      , label = Input.labelAbove [] (text "Invite this unit.")
      , options = List.map makeOption units
      }
