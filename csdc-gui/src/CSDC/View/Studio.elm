module CSDC.View.Studio exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  , Selected (..)
  )

import CSDC.API as API
import CSDC.Component.Column as Column
import CSDC.Component.DotMenu as DotMenu
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.Component.Preview as Preview
import CSDC.Component.Progress as Progress
import CSDC.Form.Unit as UnitForm
import CSDC.Form.Person as PersonForm
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.View.MessagePreview as MessagePreview
import CSDC.View.ReplyPreview as ReplyPreview
import CSDC.View.UnitPreview as UnitPreview

import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedNothing
  | SelectedUnit (Id Member)
  | SelectedInbox InboxId

type alias Model =
  { info : Maybe PersonInfo
  , panelUnits : Panel.Model (Id Member)
  , panelMessages : Panel.Model InboxId
  , notification : Notification
  , inbox : Inbox
  , selected : Selected
  , unitCreate : UnitForm.Model
  , unitCreateOpen : Bool
  , personEdit : PersonForm.Model
  , personEditOpen : Bool
  , previewMessage : MessagePreview.Model
  , previewReply : ReplyPreview.Model
  }

initial : Model
initial =
  { info = Nothing
  , panelUnits = Panel.initial "Units"
  , panelMessages = Panel.initial "Inbox"
  , notification = Notification.Empty
  , inbox = emptyInbox
  , selected = SelectedNothing
  , unitCreate = UnitForm.initial
  , unitCreateOpen = False
  , personEdit = PersonForm.initial
  , personEditOpen = False
  , previewMessage = MessagePreview.initial
  , previewReply = ReplyPreview.initial
  }

setup : Id Person -> Cmd Msg
setup id =
  Cmd.map APIMsg <|
  Cmd.batch
    [ API.getPersonInfo id
    , API.personInbox id
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | MessagesMsg (Panel.Msg InboxId)
  | MessagePreviewMsg MessagePreview.Msg
  | ReplyPreviewMsg ReplyPreview.Msg
  | UnitCreateMsg UnitForm.Msg
  | UnitCreateOpen
  | UnitCreateClose
  | PersonEditMsg PersonForm.Msg
  | PersonEditOpen
  | PersonEditClose
  | View (Id Unit)
  | CloseModal
  | Reset

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  case msg of
    Reset ->
      ({ model | notification = Notification.Empty }, Cmd.none)
    UnitsMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelUnits = Panel.update m model.panelUnits
            , selected = SelectedUnit id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelUnits = Panel.update m model.panelUnits }
          , Cmd.none
          )

    MessagesMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelMessages = Panel.update m model.panelMessages
            , selected = SelectedInbox id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelMessages = Panel.update m model.panelMessages }
          , Cmd.none
          )

    View id ->
      ( { model | selected = SelectedNothing }
      , Page.goTo pageInfo (Page.Unit id)
      )

    UnitCreateOpen ->
      ( { model | unitCreateOpen = True }
      , Cmd.none
      )

    UnitCreateClose ->
      ( { model | unitCreateOpen = False }
      , Cmd.none
      )

    UnitCreateMsg unitMsg ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just person ->
          case unitMsg of
            UnitForm.APIMsg (API.CreateUnit result) ->
              let
                initialUnitCreate = UnitForm.initial

                onSuccess = Notification.withResponse UnitForm.ResetNotification model.unitCreate

                (unitCreate, cmd) = onSuccess result <| \id ->
                  ( initialUnitCreate
                  , Page.goTo pageInfo (Page.Unit id)
                  )
              in
                ( { model | unitCreate = unitCreate, unitCreateOpen = False }
                , Cmd.map UnitCreateMsg cmd
                )

            _ ->
              let
                (unitCreate, cmd) = UnitForm.update API.createUnit person.id unitMsg model.unitCreate
              in
                ( { model | unitCreate = unitCreate }
                , Cmd.map UnitCreateMsg cmd
                )

    PersonEditOpen ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just info ->
          ( { model
            | personEditOpen = True
            , personEdit = PersonForm.fromPerson info.person
            }
          , Cmd.none
          )

    PersonEditClose ->
      ( { model | personEditOpen = False }
      , Cmd.none
      )

    PersonEditMsg personMsg ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just person ->
          case personMsg of
            PersonForm.APIMsg (API.UpdatePerson result) ->
              let
                initialPersonEdit = PersonForm.initial

                onSuccess = Notification.withResponse PersonForm.ResetNotification model.personEdit

                (personEdit, cmd) = onSuccess result <| \_ ->
                  ( initialPersonEdit
                  , Page.goTo pageInfo Page.Studio
                  )
              in
                ( { model | personEdit = personEdit, personEditOpen = False }
                , Cmd.map PersonEditMsg cmd
                )

            _ ->
              let
                (personEdit, cmd) = PersonForm.update (API.updatePerson person.id) person.person.orcid personMsg model.personEdit
              in
                ( { model | personEdit = personEdit }
                , Cmd.map PersonEditMsg cmd
                )

    MessagePreviewMsg preMsg ->
      case model.selected of
        SelectedInbox inboxId ->
          case inboxId of
            MessageMemberId id ->
              let
                (previewMessage, cmd) = MessagePreview.updateMember id preMsg model.previewMessage
              in
                case preMsg of
                  MessagePreview.APIMsg (API.SendReplyMember (Ok _)) ->
                    ( { model | previewMessage = previewMessage, selected = SelectedNothing }
                    , Cmd.batch
                        [ Page.goTo pageInfo Page.Studio
                        , Cmd.map MessagePreviewMsg cmd
                        ]
                    )
                  _ ->
                    ( { model | previewMessage = previewMessage }
                    , Cmd.map MessagePreviewMsg cmd
                    )

            MessageSubpartId id ->
              let
                (previewMessage, cmd) = MessagePreview.updateSubpart id preMsg model.previewMessage
              in
                case preMsg of
                  MessagePreview.APIMsg (API.SendReplySubpart (Ok _)) ->
                    ( { model | previewMessage = previewMessage, selected = SelectedNothing }
                    , Cmd.batch
                        [ Page.goTo pageInfo Page.Studio
                        , Cmd.map MessagePreviewMsg cmd
                        ]
                    )
                  _ ->
                    ( { model | previewMessage = previewMessage }
                    , Cmd.map MessagePreviewMsg cmd
                    )

            _ ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    ReplyPreviewMsg preMsg ->
      case model.selected of
        SelectedInbox inboxId ->
          case inboxId of
            ReplyMemberId id ->
              let
                (previewReply, cmd) = ReplyPreview.updateMember id preMsg model.previewReply
              in
                case preMsg of
                  ReplyPreview.APIMsg (API.ViewReplyMember (Ok _)) ->
                    ( { model | previewReply = previewReply, selected = SelectedNothing }
                    , Cmd.batch
                        [ Page.goTo pageInfo Page.Studio
                        , Cmd.map ReplyPreviewMsg cmd
                        ]
                    )
                  _ ->
                    ( { model | previewReply = previewReply }
                    , Cmd.map ReplyPreviewMsg cmd
                    )

            ReplySubpartId id ->
              let
                (previewReply, cmd) = ReplyPreview.updateSubpart id preMsg model.previewReply
              in
                case preMsg of
                  ReplyPreview.APIMsg (API.ViewReplySubpart (Ok _)) ->
                    ( { model | previewReply = previewReply, selected = SelectedNothing }
                    , Cmd.batch
                        [ Page.goTo pageInfo Page.Studio
                        , Cmd.map ReplyPreviewMsg cmd
                        ]
                    )
                  _ ->
                    ( { model | previewReply = previewReply }
                    , Cmd.map ReplyPreviewMsg cmd
                    )

            _ ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    CloseModal ->
      ( { model | selected = SelectedNothing }
      , Cmd.none
      )

    APIMsg apimsg ->
      let
        onSuccess = Notification.withResponse Reset model
      in
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
              ( { model | info = Just info, panelUnits = panelUnits }
              , Cmd.none
              )

        API.PersonInbox result -> onSuccess result <| \inbox ->
            let
              panelMessages =
                Panel.update (Panel.SetItems <| inboxToItems inbox) model.panelMessages
            in
              ( { model | inbox = inbox, panelMessages = panelMessages }
              , Cmd.none
              )

        API.SendReplyMember result -> onSuccess result <| \_ ->
            case model.info of
              Nothing ->
                ( model
                , Cmd.none
                )
              Just info ->
                ( model
                , setup info.id
                )

        API.ViewReplyMember result -> onSuccess result <| \_ ->
            case model.info of
              Nothing ->
                ( model
                , Cmd.none
                )
              Just info ->
                ( model
                , setup info.id
                )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

dotMenu : List (DotMenu.Item Msg)
dotMenu =
  [ { label = "Edit Profile"
    , message = PersonEditOpen
    }
  , { label = "Create New Unit"
    , message = UnitCreateOpen
    }
  ]

view : Model -> List (Html Msg)
view model =
  case model.info of
    Nothing ->
      [ Progress.view
      ] ++ Notification.view model.notification

    Just info ->
      [ Html.h1
          [ Html.Attributes.class "title" ]
          [ Html.text "Studio" ]
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.make "Information" dotMenu
                  [ Html.div
                      [ Html.Attributes.class "media"
                      , Html.Attributes.style "padding-bottom" "25px"
                      ]
                      [ Html.div
                          [ Html.Attributes.class "media-left" ]
                          [ Html.figure
                              [ Html.Attributes.class "image is-48x48"
                              , Html.Attributes.style "margin" "0"
                              ]
                              [ Html.img
                                  [ Html.Attributes.src "https://bulma.io/images/placeholders/96x96.png"
                                  , Html.Attributes.alt "Profile image"
                                  ]
                                  []
                              ]
                          ]
                      , Html.div
                          [ Html.Attributes.class "media-content" ]
                          [ Html.p
                              [ Html.Attributes.class "title is-5" ]
                              [ Html.text info.person.name ]
                          , Html.p
                              [ Html.Attributes.class "subtitle is-6" ]
                              [ Html.text "ORCID: "
                              , Html.a
                                  [ Html.Attributes.href ("https://orcid.org/" ++ info.person.orcid)
                                  , Html.Attributes.target "_blank"
                                  ]
                                  [ Html.text info.person.orcid ]
                              ]
                          ]
                      ]
                  , Html.div
                      [ Html.Attributes.class "content"]
                      [ Html.text info.person.description ]
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map UnitsMsg <| Panel.view model.panelUnits ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map MessagesMsg <| Panel.view model.panelMessages ]
          ]

      , Modal.view model.personEditOpen PersonEditClose <| List.singleton <|
          Html.map PersonEditMsg <|
          Preview.make
            [ Html.h2
                []
                [ Html.text "Edit Profile" ]
            , PersonForm.view model.personEdit
            ]

      , Modal.view model.unitCreateOpen UnitCreateClose <| List.singleton <|
          Html.map UnitCreateMsg <|
          Preview.make
            [ Html.h2
                []
                [ Html.text "Create Unit" ]
            , UnitForm.view model.unitCreate
            ]

      , let
          isActive = case model.selected of
            SelectedNothing -> False
            _ -> True
        in
          Modal.view isActive CloseModal <| List.singleton <|
            case model.selected of
              SelectedNothing ->
                Html.div [] []

              SelectedUnit id ->
                case idMapLookup id info.members of
                  Nothing ->
                    Html.text "Error."
                  Just unit ->
                    UnitPreview.view unit.value (View unit.id)

              SelectedInbox inboxId ->
                case inboxId of
                  ReplyMemberId id ->
                    case idMapLookup id model.inbox.replyMember of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        Html.map ReplyPreviewMsg (ReplyPreview.view msg)

                  MessageMemberId id ->
                    case idMapLookup id model.inbox.messageMember of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        Html.map MessagePreviewMsg (MessagePreview.view msg model.previewMessage)

                  MessageSubpartId id ->
                    case idMapLookup id model.inbox.messageSubpart of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        Html.map MessagePreviewMsg (MessagePreview.view msg model.previewMessage)

                  ReplySubpartId id ->
                    case idMapLookup id model.inbox.replySubpart of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        Html.map ReplyPreviewMsg (ReplyPreview.view msg)

      ] ++ Notification.view model.notification

--------------------------------------------------------------------------------
-- Helpers

type InboxId
  = MessageMemberId (Id (Message Member))
  | ReplyMemberId (Id (Reply Member))
  | MessageSubpartId (Id (Message Subpart))
  | ReplySubpartId (Id (Reply Subpart))

inboxToItems : Inbox -> List (Panel.Item InboxId)
inboxToItems inbox =
  let
    fmm (id, m) =
      { index = MessageMemberId id
      , title = "Invitation from " ++ m.right
      , description = m.text
      }
    frm (id, r) =
      { index = ReplyMemberId id
      , title = case r.mtype of
          Invitation -> "Reply from " ++ r.message.left
          Submission -> "Reply from " ++ r.message.right
      , description = r.text
      }
    fms (id, m) =
      { index = MessageSubpartId id
      , title = "Submission from " ++ m.left
      , description = m.text
      }
    frs (id, r) =
      { index = ReplySubpartId id
      , title = case r.mtype of
          Invitation -> "Reply from " ++ r.message.left
          Submission -> "Reply from " ++ r.message.right
      , description = r.text
      }
  in
    List.map fmm (idMapToList inbox.messageMember) ++
    List.map frm (idMapToList inbox.replyMember) ++
    List.map fms (idMapToList inbox.messageSubpart) ++
    List.map frs (idMapToList inbox.replySubpart)
