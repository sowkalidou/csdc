module CSDC.View.Person exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.UI.Column as Column
import CSDC.UI.DotMenu as DotMenu
import CSDC.UI.Modal as Modal
import CSDC.UI.BoxImageText as BoxImageText
import CSDC.UI.PreviewImageText as PreviewImageText
import CSDC.UI.Progress as Progress
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Form.Message as MessageForm
import CSDC.Page as Page
import CSDC.Types exposing (..)
import Form

import Html exposing (Html)
import Html.Attributes
import Markdown

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : Maybe PersonInfo
  , selectedUnit : Maybe (Id Unit)
  , messageCreate : MessageForm.Model
  , messageCreateOpen : Bool
  , notification : Notification
  }

initial : Model
initial =
  { person = Nothing
  , selectedUnit = Nothing
  , messageCreate = MessageForm.initial
  , messageCreateOpen = False
  , notification = Notification.Empty
  }

setup : Id Person -> Cmd Msg
setup id = Cmd.map APIMsg <| API.getPersonInfo id

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg (API.Response PersonInfo)
  | SetSelectedUnit (Id Unit)
  | ViewSelectedUnit (Id Unit)
  | MessageCreateMsg (MessageForm.Msg NewMember)
  | MessageCreateOpen
  | MessageCreateClose
  | Reset
  | CloseModal

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    SetSelectedUnit uid ->
      ( { model | selectedUnit = Just uid }
      , Cmd.none
      )

    ViewSelectedUnit uid ->
      ( initial
      , Page.goTo pageInfo (Page.Unit uid)
      )

    CloseModal ->
      ( { model | selectedUnit = Nothing }
      , Cmd.none
      )

    MessageCreateOpen ->
      ( { model
        | messageCreateOpen = True
        , messageCreate = case model.person of
            Nothing -> MessageForm.initial
            Just info -> MessageForm.fromPersonInfo info
        }
      , Cmd.none
      )

    MessageCreateClose ->
      ( { model | messageCreateOpen = False }
      , Cmd.none
      )

    MessageCreateMsg messageMsg ->
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

    APIMsg result -> Notification.withResponse Reset model result <| \info ->
      ( { model | person = Just info }
      , Cmd.none
      )

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
              [ Column.view "Information"
                  [ DotMenu.make <|
                    if List.isEmpty person.unitsForMessage
                    then
                      []
                    else
                      [ { label = "Invite this person to your unit"
                        , message = MessageCreateOpen
                        }
                      ]
                  ]
                  [ Html.div
                      [ Html.Attributes.class "media"
                      , Html.Attributes.style "padding-bottom" "25px"
                      ]
                      [ Html.div
                          [ Html.Attributes.class "media-left" ]
                          [ Html.figure
                              [ Html.Attributes.class "image is-96x96"
                              , Html.Attributes.style "margin" "0"
                              ]
                              [ Html.img
                                  [ Html.Attributes.src person.person.image
                                  , Html.Attributes.style "border-radius" "10%"
                                  , Html.Attributes.alt "Profile photo"
                                  ]
                                  []
                              ]
                          ]
                      , Html.div
                          [ Html.Attributes.class "media-content"
                          , Html.Attributes.style "padding-top" "24px"
                          ]
                          [ Html.p
                              [ Html.Attributes.class "title is-5" ]
                              [ Html.text person.person.name ]
                          , Html.p
                              [ Html.Attributes.class "subtitle is-6" ]
                              [ Html.text "ORCID: "
                              , Html.a
                                  [ Html.Attributes.href ("https://orcid.org/" ++ person.person.orcid)
                                  , Html.Attributes.target "_blank"
                                  ]
                                  [ Html.text person.person.orcid ]
                              ]
                          ]
                      ]

                  , Markdown.toHtml [] person.person.description
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view "Units" [] (viewUnits person) ]
          ]

      , Modal.view model.messageCreateOpen MessageCreateClose <|
          Html.map MessageCreateMsg <|
          let
            make uid = { person = person.id, unit = uid }
          in
            Form.viewWith "Send Invitation" (MessageForm.view Invitation make) model.messageCreate

      , Modal.viewMaybe model.selectedUnit CloseModal <| \id ->
          case lookupById id person.members of
            Nothing ->
              Html.text "Error."
            Just personMember ->
              PreviewImageText.view personMember.unit (ViewSelectedUnit personMember.id)

      ] ++ Notification.view model.notification

viewUnits : PersonInfo -> List (Html Msg)
viewUnits info =
  let
    toBox member =
      Html.map SetSelectedUnit <|
      BoxImageText.view False member.id member.unit
  in
    List.map toBox info.members
