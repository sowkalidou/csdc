module CSDC.View.Person exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Column as Column
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.Component.Progress as Progress
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.View.UnitPreview as UnitPreview
import CSDC.Form.Message as MessageForm
import CSDC.Page as Page
import CSDC.Types exposing (..)
import Form

import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe PersonInfo
  , panelUnits : Panel.Model (Id Member)
  , messageCreate : MessageForm.Model
  , messageCreateOpen : Bool
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , panelUnits = Panel.initial "Units"
  , messageCreate = MessageForm.initial
  , messageCreateOpen = False
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
  | MessageCreateMsg (MessageForm.Msg Member)
  | MessageCreateOpen
  | MessageCreateClose
  | Reset
  | CloseModal

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    UnitsMsg m ->
      ( { model | panelUnits = Panel.update m model.panelUnits }
      , Cmd.none
      )

    ViewSelected uid ->
      ( initial
      , Page.goTo pageInfo (Page.Unit uid)
      )

    CloseModal ->
      ( { model | panelUnits = Panel.update (Panel.SetSelected Nothing) model.panelUnits }
      , Cmd.none
      )

    MessageCreateOpen ->
      ( { model | messageCreateOpen = True }
      , Cmd.map MessageCreateMsg MessageForm.setup
      )

    MessageCreateClose ->
      ( { model | messageCreateOpen = False }
      , Cmd.none
      )

    MessageCreateMsg messageMsg ->
      case model.person of
        Nothing -> (model, Cmd.none)
        Just person ->
          let
            config =
              { request = API.sendMessageMember
              , finish = Cmd.none
              }
            (messageCreate, cmd) = MessageForm.updateWith config messageMsg model.messageCreate
          in
            ( { model
              | messageCreate = messageCreate
              , messageCreateOpen = not (Form.isFinished messageMsg)
              }
            , Cmd.map MessageCreateMsg cmd
            )

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    APIMsg apimsg ->
      let onSuccess = Notification.withResponse Reset model in
      case apimsg of
        API.GetPersonInfo result -> onSuccess result <| \info ->
          let
            pairs =
              idMapToList info.members |>
              List.map (\(uid,unit) ->
                { index = uid
                , title = unit.value.name
                , description = unit.value.description
                }
              )

            panelUnits = Panel.update (Panel.SetItems pairs) model.panelUnits
          in
            ( { model | person = Just info, panelUnits = panelUnits }
            , Cmd.none
            )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  case model.person of
    Nothing ->
      [ Progress.view
      ] ++ Notification.view model.notification

    Just person ->
      [ Html.h1
          [ Html.Attributes.class "title" ]
          [ Html.text person.person.name ]
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-two-thirds" ]
              [ Column.make "Information"
                  [ { label = "Invite this person to your unit"
                    , message = MessageCreateOpen
                    }
                  ]
                  [ Html.div
                      []
                      [ Html.strong [] [ Html.text "ORCID ID: " ]
                      , Html.a
                          [ Html.Attributes.target "_blank" ]
                          [ Html.text ("https://orcid.org/" ++ person.person.orcid) ]
                      ]

                  , Html.div
                      [ Html.Attributes.style "white-space" "pre-wrap"
                      ]
                      [ Html.strong [] [ Html.text "Description: " ]
                      , Html.text person.person.description
                      ]
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map UnitsMsg <| Panel.view model.panelUnits ]
          ]

      , Modal.view model.messageCreateOpen MessageCreateClose <|
          Html.map MessageCreateMsg <|
          let
            make uid = { person = person.id, unit = uid }
          in
            Form.viewWith "Send Invitation" (MessageForm.view Invitation make) model.messageCreate

      , let
          isActive =
            case model.panelUnits.selected of
              Nothing -> False
              _ -> True
        in
          Modal.view isActive CloseModal <|
            case model.panelUnits.selected of
              Nothing ->
                Html.div [] []
              Just id ->
                case idMapLookup id person.members of
                  Nothing ->
                    Html.text "Error."
                  Just unit ->
                    UnitPreview.view unit.value (ViewSelected unit.id)
      ] ++ Notification.view model.notification
