module CSDC.Component.Studio exposing
  ( Model
  , initial
  , setup
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe Person
  , units : Panel.Model (Id Unit)
  , messages : Panel.Model Int -- todo: actually implement messages
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , units = Panel.initial "Units"
  , messages = Panel.initial "Messages"
  , notification = Notification.Empty
  }

setup : Id Person -> Cmd Msg
setup id =
  Cmd.batch
    [ Cmd.map APIMsg <| API.selectPerson id
    , Cmd.map APIMsg <| API.selectMemberPerson id
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Unit))
  | MessagesMsg (Panel.Msg Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UnitsMsg m ->
      ( { model | units = Panel.update m model.units }
      , Cmd.none
      )

    MessagesMsg m ->
      ( { model | messages = Panel.update m model.messages }
      , Cmd.none
      )

    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok person ->
              ( { model | person = Just person }
              , Cmd.none
              )

        API.SelectMemberPerson result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok idmap ->
              let
                pairs =
                  idMapToList idmap |>
                  List.map (\(_,member) -> (member.unit, idToString member.unit))

                units = Panel.update (Panel.SetItems pairs) model.units
              in
              ( { model | units = units }
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
          [ text "Studio" ]
      , row []
          [ text person.name ]
      , row []
          [ el [ Font.bold ] (text "ORCID ID: ")
          , newTabLink []
              { url = "https://orcid.org/" ++ person.orcid
              , label = text person.orcid
              }
          ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map UnitsMsg <| Panel.view model.units
          , map MessagesMsg <| Panel.view model.messages
          ]
      , row
          [ height <| fillPortion 1
          , width fill
          ]
          [ text "Placeholder" ]
      ] ++
      Notification.view model.notification
