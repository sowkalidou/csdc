module CSDC.Component.MessageSubpart exposing
  ( Param
  , Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Input exposing (button)
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
  , selected : Maybe (Id Unit)
  , text : String
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , unit = Nothing
  , text = ""
  , selected = Nothing
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
  | SelectInvitation (Id Unit)
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

    SelectInvitation id ->
      ( { model | selected = Just id }
      , Cmd.none
      )

    Submit ->
      case model.selected of
        Nothing ->
          ( { model | notification = Notification.Error ["Please select a unit."] }
          , Cmd.none
          )

        Just selected ->
          case model.unit of
            Nothing ->
              (model, Cmd.none)
            Just unit ->
              let
                message = Message
                 { mtype = param.messageType
                 , text = model.text
                 , status = Waiting
                 , value =
                     case param.messageType of
                       Invitation ->
                         Subpart selected unit.id
                       Submission ->
                         Subpart unit.id selected
                 }
              in
                ( { model | notification = Notification.Processing }
                , Cmd.map APIMsg <| API.sendMessageSubpart message
                )

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
          ( { initial | notification = Notification.Success }
          , Cmd.batch
              [ Notification.reset Reset
              , case model.unit of
                  Nothing -> Cmd.none
                  Just unit -> Page.goTo pageInfo (Page.ViewUnit unit.id)
              ]
          )

        API.SendMessageSubpart result -> onSuccess result <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.batch
              [ Notification.reset Reset
              , case model.unit of
                  Nothing -> Cmd.none
                  Just unit -> Page.goTo pageInfo (Page.ViewUnit unit.id)
              ]
          )

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
              text <| "Invitation of " ++ unitInfo.unit.name
        ]
    , let
        units =
          List.filter (eligible param.messageType unitInfo) <|
          personInfoChair personInfo
      in
        if List.isEmpty units
        then text "You must be the chair of a unit to send messages to units."
        else invitation model.selected units SelectInvitation

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
  Maybe (Id Unit) ->
  List (WithId Unit) ->
  (Id Unit -> msg) ->
  Element msg
invitation selected units makeEvent =
  let
    makeOption unit = Input.option unit.id (text unit.value.name)
  in
    Input.radioRow
      [ padding 10
      , spacing 20
      ]
      { onChange = makeEvent
      , selected = selected
      , label = Input.labelAbove [] (text "Relevant unit.")
      , options = List.map makeOption units
      }

--------------------------------------------------------------------------------
-- Helper

eligible : MessageType -> UnitInfo -> WithId Unit -> Bool
eligible mtype info unit =
  case mtype of
    Invitation ->
      not <| idMapAny (\p -> p.id == unit.id) info.parents
    Submission ->
      not <| idMapAny (\p -> p.id == unit.id) info.children
