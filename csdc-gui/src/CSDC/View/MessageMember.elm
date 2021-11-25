module CSDC.View.MessageMember exposing
  ( Param
  , Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
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
  { messageType : MessageType
  }

type alias Model =
  { person : Maybe PersonInfo
  , unit : Maybe UnitInfo
  , text : String
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , unit = Nothing
  , text = ""
  , notification = Notification.Empty
  }

setup : Id Person -> Id Unit -> Cmd Msg
setup pid uid = Cmd.batch
  [ Cmd.map APIMsg <| API.getPersonInfo pid
  , Cmd.map APIMsg <| API.getUnitInfo uid
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputText String
  | APIMsg API.Msg
  | Submit
  | Reset

update : Page.Info -> Msg -> Param -> Model -> (Model, Cmd Msg)
update pageInfo msg param model =
  case msg of
    InputText text ->
      ( { model | text = text }
      , Cmd.none
      )

    Submit ->
      case (model.person, model.unit) of
        (Just personInfo, Just unitInfo) ->
          let
            message =
             { mtype = param.messageType
             , text = model.text
             , status = Waiting
             , value = { person = personInfo.id, unit = unitInfo.id }
             }
          in
            ( { model | notification = Notification.Processing }
            , Cmd.map APIMsg <| API.sendMessageMember message
            )
        _ ->
          (model, Cmd.none)

    APIMsg apimsg ->
      let
        onSuccess = Notification.withResponse Reset model
      in
      case apimsg of
        API.GetPersonInfo result -> onSuccess result <| \person ->
          ( { model | person = Just person }
          , Cmd.none
          )

        API.GetUnitInfo result -> onSuccess result <| \unit ->
          ( { model | unit = Just unit }
          , Cmd.none
          )

        API.SendMessageMember result -> onSuccess result <| \_ ->
          case model.unit of
            Just unitInfo ->
              ( { initial | notification = Notification.Success }
              , Cmd.batch
                  [ Notification.reset Reset
                  , Page.goTo pageInfo (Page.Unit unitInfo.id)
                  ]
              )

            Nothing ->
              (model, Cmd.none)

        _ ->
          (model, Cmd.none)

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

whenLoaded : Model -> (PersonInfo -> UnitInfo -> Element msg) -> Element msg
whenLoaded model makeView =
  case (model.person, model.unit) of
    (Just person, Just unit) -> makeView person unit
    _ -> text "Loading..."

view : Param -> Model -> Element Msg
view param model = whenLoaded model <| \personInfo unitInfo ->
  column [ width <| fillPortion 2, padding 10, spacing 10 ] <|
    [ row
        [ Font.bold, Font.size 30 ]
        [ case param.messageType of
            Submission ->
              text <| "Submission for " ++ unitInfo.unit.name
            Invitation ->
              text <| "Invitation for " ++ personInfo.person.name
        ]
    , Input.multiline []
        { label = Input.labelAbove [] (text "Your message.")
        , onChange = InputText
        , placeholder = Nothing
        , text = model.text
        , spellcheck = True
        }
    , Element.html <| CSDC.Input.button Submit "Submit"
    ] ++ List.map html (Notification.view model.notification)

