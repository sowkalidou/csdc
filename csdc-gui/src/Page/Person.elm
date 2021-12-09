module Page.Person exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.Column as Column
import UI.DotMenu as DotMenu
import UI.Modal as Modal
import UI.BoxImageText as BoxImageText
import UI.PreviewImageText as PreviewImageText
import Notification exposing (Notification)
import Form.Message as MessageForm
import Page as Page
import Types exposing (..)
import Form
import WebData exposing (WebData)

import Html exposing (Html)
import Html.Attributes
import Markdown

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { person : WebData PersonInfo
  , selectedUnit : Maybe (Id Unit)
  , messageCreate : MessageForm.Model
  , messageCreateOpen : Bool
  , notification : Notification
  }

initial : Model
initial =
  { person = WebData.Loading
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
      , Page.goTo pageInfo (Page.Unit Page.UnitInfo uid)
      )

    CloseModal ->
      ( { model | selectedUnit = Nothing }
      , Cmd.none
      )

    MessageCreateOpen -> WebData.update model model.person <| \info ->
      ( { model
        | messageCreateOpen = True
        , messageCreate = MessageForm.fromPersonInfo info
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
      ( { model | person = WebData.Success info }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  Notification.with model.notification <|
  WebData.view model.person <| \person ->
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
                              [ Html.Attributes.src <| filePath person.person.image
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

  ]

viewUnits : PersonInfo -> List (Html Msg)
viewUnits info =
  let
    toBox member =
      Html.map SetSelectedUnit <|
      BoxImageText.view False [] member.id member.unit
  in
    List.map toBox info.members
