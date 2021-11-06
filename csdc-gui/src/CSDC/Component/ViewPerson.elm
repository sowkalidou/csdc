module CSDC.Component.ViewPerson exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe PersonInfo
  , panelUnits : Panel.Model (Id Member)
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , panelUnits = Panel.initial "Units"
  , notification = Notification.Empty
  }

setup : Id Person -> Cmd Msg
setup id = Cmd.map APIMsg <| API.getPersonInfo id

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | ViewSelected (Id Unit)
  | MessageMember (Id Person)

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    UnitsMsg m ->
      ( { model | panelUnits = Panel.update m model.panelUnits }
      , Cmd.none
      )

    ViewSelected uid ->
      ( model
      , Page.goTo pageInfo (Page.ViewUnit uid)
      )

    MessageMember pid ->
      ( model
      , Page.goTo pageInfo (Page.InvitationMember pid)
      )

    APIMsg apimsg ->
      case apimsg of
        API.GetPersonInfo result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | person = Just person }
              , Cmd.none
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  case model.person of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just person ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "View Person" ]
      , row []
          [ text person.person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ person.person.orcid
              , label = text person.person.orcid
              }
          ]

      , row []
          [ button (MessageMember person.id) "Invite this person to your unit"
          ]

      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ column
              [ width <| fillPortion 1 ]
              [ text person.person.description ]
          , map UnitsMsg <| Panel.view model.panelUnits
          ]
      , case Panel.getSelected model.panelUnits of
          Nothing ->
            row [] []
          Just id ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case idMapLookup id person.members of
                Nothing ->
                  [ text "Error." ]
                Just unit ->
                  PreviewUnit.view unit.value (ViewSelected unit.id)
      ] ++
      Notification.view model.notification
