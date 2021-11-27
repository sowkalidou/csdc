module CSDC.View.Admin.NewMessage exposing
  ( Model
  , initial
  , Msg
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
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Field String (Id Person)
  , unit : Field String (Id Unit)
  , messageType : Field (Maybe MessageType) MessageType
  , notification : Notification
  }

initial : Model
initial =
  { person = Field.requiredId "Person"
  , unit = Field.requiredId "Unit"
  , messageType = Field.required "Message Type"
  , notification = Notification.Empty
  }

validate : Model -> Result (List String) (NewMessage Member)
validate model =
  let
    result =
      Validation.valid NewMessage
        |> Validation.andMap (Field.validate model.messageType)
        |> Validation.andMap (Validation.valid "Message")
        |> Validation.andMap
             ( Validation.valid Member
                 |> Validation.andMap (Field.validate model.person)
                 |> Validation.andMap (Field.validate model.unit)
             )
  in
    Validation.validate result

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputPerson String
  | InputUnit String
  | APIMsg (API.Response (Id (Message Member)))
  | InputMessageType MessageType
  | Submit
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputPerson str ->
      ( { model | person = Field.set str model.person }
      , Cmd.none
      )

    InputUnit str ->
      ( { model | unit = Field.set str model.unit }
      , Cmd.none
      )

    Submit ->
      case validate model of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok message ->
          ( { model | notification = Notification.Processing }
          , Cmd.map APIMsg <| API.sendMessageMember message
          )

    InputMessageType messageType ->
      ( { model | messageType = Field.set (Just messageType) model.messageType }
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
        [ text "New Message" ]
    , selectMessageType model
    , Input.text
        { onChange = InputPerson
        , field = model.person
        }
    , Input.text
        { onChange = InputUnit
        , field = model.unit
        }
    , Input.button Submit "Submit"
    ] ++ List.map html (Notification.view model.notification)

selectMessageType : Model -> Element Msg
selectMessageType model =
  Input.radio
    { onChange = InputMessageType
    , field = model.messageType
    , options =
        [ (Invitation, "Invitation")
        , (Submission, "Submission")
        ]
    }

