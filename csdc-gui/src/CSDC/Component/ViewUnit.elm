module CSDC.Component.ViewUnit exposing
  ( Model
  , initial
  , Msg (..)
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
  { id : Maybe (Id Unit)
  , unit : Maybe Unit
  , members : IdMap Member (WithId Person)
  , subparts : IdMap Subpart (WithId Unit)
  , panelSubparts : Panel.Model (Id Subpart)
  , panelMembers : Panel.Model (Id Member)
  , notification : Notification
  }

initial : Model
initial =
  { id = Nothing
  , unit = Nothing
  , subparts = idMapEmpty
  , members = idMapEmpty
  , panelSubparts = Panel.initial "Sub-Units"
  , panelMembers = Panel.initial "Members"
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | SubpartsMsg (Panel.Msg (Id Subpart))
  | MembersMsg (Panel.Msg (Id Member))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SubpartsMsg m ->
      ( { model | panelSubparts = Panel.update m model.panelSubparts }
      , Cmd.none
      )

    MembersMsg m ->
      ( { model | panelMembers = Panel.update m model.panelMembers }
      , Cmd.none
      )

    APIMsg apimsg ->
      case apimsg of
        API.SelectUnit id result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok unit ->
              ( { model | id = Just id, unit = Just unit }
              , Cmd.batch
                  [ Cmd.map APIMsg <| API.getUnitMembers id
                  , Cmd.map APIMsg <| API.getUnitSubparts id
                  ]
              )

        API.GetUnitMembers result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok members ->
              let
                pairs =
                  idMapToList members |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelMembers = Panel.update (Panel.SetItems pairs) model.panelMembers
              in
                ( { model | members = members, panelMembers = panelMembers }
                , Cmd.none
                )

        API.GetUnitSubparts result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )
            Ok subparts ->
              let
                pairs =
                  idMapToList subparts |>
                  List.map (\(id,withid) -> (id, withid.value.name))

                panelSubparts = Panel.update (Panel.SetItems pairs) model.panelSubparts
              in
                ( { model | subparts = subparts, panelSubparts = panelSubparts }
                , Cmd.none
                )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  case model.unit of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just unit ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Viewer" ]
      , row []
          [ text unit.name ]
      , row []
          [ text unit.description ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map SubpartsMsg <| Panel.view model.panelSubparts
          , map MembersMsg <| Panel.view model.panelMembers
          ]
      , case Panel.getSelected model.panelSubparts of
          Nothing ->
            row [] []
          Just id ->
            row
              [ height <| fillPortion 1
              , width fill
              ]
              [ case idMapLookup id model.subparts of
                  Nothing -> text "Error."
                  Just subunit -> text subunit.value.name
              ]
      ] ++
      Notification.view model.notification
