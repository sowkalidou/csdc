module CSDC.View.InvitationMember exposing
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
  { person : Id Person
  , user : PersonInfo
  }

type alias Model =
  { text : String
  , unit : Maybe (Id Unit)
  , notification : Notification
  , person : Maybe PersonInfo
  }

initial : Model
initial =
  { text = ""
  , unit = Nothing
  , notification = Notification.Empty
  , person = Nothing
  }

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
      ( { model | unit = Just id }
      , Cmd.none
      )

    Submit ->
      case model.unit of
        Nothing ->
          ( { model | notification = Notification.Error ["Please choose a unit."] }
          , Cmd.none
          )

        Just unit ->
          let
            message = Message
             { mtype = Invitation
             , text = model.text
             , status = Waiting
             , value = makeMember param.person unit
             }
          in
            ( { model | notification = Notification.Processing }
            , Cmd.map APIMsg <| API.sendMessageMember message
            )

    APIMsg apimsg ->
      let
        onSuccess = Notification.withResponse Reset model
      in
      case apimsg of
        API.SendMessageMember result -> onSuccess result <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.batch
              [ Notification.reset Reset
              , case model.unit of
                  Nothing -> Cmd.none
                  Just unit -> Page.goTo pageInfo (Page.Unit unit)
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

view : Param -> Model -> Element Msg
view param model =
  case model.person of
    Nothing -> column [] []
    Just person ->
      column [ width <| fillPortion 2, padding 10, spacing 10 ] <|
        [ row
            [ Font.bold, Font.size 30 ]
            [ text <| "Invitation for " ++ person.person.name ]
        , let
            units =
              List.filter (eligible person) <|
              personInfoChair param.user
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
        , Element.html <| CSDC.Input.button Submit "Submit"
        ] ++ List.map html (Notification.view model.notification)

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

eligible : PersonInfo -> WithId Unit -> Bool
eligible info unit =
  not <| idMapAny (\p -> p.id == unit.id) info.members
