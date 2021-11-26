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
import CSDC.Form.ReplySeen as ReplySeenForm
import CSDC.Form.Reply as ReplyForm
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.View.UnitPreview as UnitPreview
import Form

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
  , previewMessage : ReplyForm.Model
  , previewReply : ReplySeenForm.Model
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
  , previewMessage = ReplyForm.initial
  , previewReply = ReplySeenForm.initial
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
  | ReplyMsg ReplyForm.Msg
  | ReplySeenMsg ReplySeenForm.Msg
  | UnitCreateMsg (UnitForm.Msg (Id Unit))
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
        Nothing -> (model, Cmd.none)
        Just person ->
          let
            config =
              { request = API.createUnit
              , finish = \id -> Page.goTo pageInfo (Page.Unit id)
              }
            (unitCreate, cmd) = UnitForm.updateWith config unitMsg model.unitCreate
          in
            ( { model
              | unitCreate = unitCreate
              , unitCreateOpen = not (Form.isFinished unitMsg)
              }
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
        Nothing -> (model, Cmd.none)
        Just person ->
          let
            config =
              { id = person.id
              , finish = Page.goTo pageInfo Page.Studio
              }
            (personEdit, cmd) = PersonForm.updateWith config personMsg model.personEdit
          in
            ( { model
              | personEdit = personEdit
              , personEditOpen = not (Form.isFinished personMsg)
              }
            , Cmd.map PersonEditMsg cmd
            )

    ReplyMsg preMsg ->
      case model.selected of
        SelectedInbox inboxId ->
          case inboxId of
            MessageMemberId id ->
              let
                config =
                  { request = \(rtype, reason) ->
                      API.sendReplyMember { rtype = rtype, text = reason, message = id }
                  , finish = Page.goTo pageInfo Page.Studio
                  }

                (previewMessage, cmd) = ReplyForm.updateWith config preMsg model.previewMessage
              in
                ( { model
                  | previewMessage = previewMessage
                  , selected =
                      if Form.isFinished preMsg then SelectedNothing else model.selected
                  }
                , Cmd.map ReplyMsg cmd
                )

            MessageSubpartId id ->
              let
                config =
                  { request = \(rtype, reason) ->
                      API.sendReplySubpart { rtype = rtype, text = reason, message = id }
                  , finish = Page.goTo pageInfo Page.Studio
                  }

                (previewMessage, cmd) = ReplyForm.updateWith config preMsg model.previewMessage
              in
                ( { model
                  | previewMessage = previewMessage
                  , selected =
                      if Form.isFinished preMsg then SelectedNothing else model.selected
                  }
                , Cmd.map ReplyMsg cmd
                )

            _ ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    ReplySeenMsg preMsg ->
      case model.selected of
        SelectedInbox inboxId ->
          case inboxId of
            ReplyMemberId id ->
              let
                config =
                  { request = API.viewReplyMember id
                  , finish = Page.goTo pageInfo Page.Studio
                  }

                (previewReply, cmd) = ReplySeenForm.updateWith config preMsg model.previewReply
              in
                ( { model
                  | previewReply = previewReply
                  , selected =
                      if Form.isFinished preMsg then SelectedNothing else model.selected
                  }
                , Cmd.map ReplySeenMsg cmd
                )

            ReplySubpartId id ->
              let
                config =
                  { request = API.viewReplySubpart id
                  , finish = Page.goTo pageInfo Page.Studio
                  }

                (previewReply, cmd) = ReplySeenForm.updateWith config preMsg model.previewReply
              in
                ( { model
                  | previewReply = previewReply
                  , selected =
                      if Form.isFinished preMsg then SelectedNothing else model.selected
                  }
                , Cmd.map ReplySeenMsg cmd
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

      , Modal.view model.personEditOpen PersonEditClose <|
          Html.map PersonEditMsg <|
          Form.viewWith "Edit Profile" PersonForm.view model.personEdit

      , Modal.view model.unitCreateOpen UnitCreateClose <|
          Html.map UnitCreateMsg <|
          Form.viewWith "Create Unit" UnitForm.view model.unitCreate

      , let
          isActive = case model.selected of
            SelectedNothing -> False
            _ -> True
        in
          Modal.view isActive CloseModal <|
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
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation Reply"
                            Submission -> "Submission Reply"
                        in
                          Html.map ReplySeenMsg <|
                          Form.viewWith title (ReplySeenForm.view msg) model.previewReply

                  MessageMemberId id ->
                    case idMapLookup id model.inbox.messageMember of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation"
                            Submission -> "Submission"
                        in
                          Html.map ReplyMsg <|
                          Form.viewWith title (ReplyForm.view msg) model.previewMessage

                  MessageSubpartId id ->
                    case idMapLookup id model.inbox.messageSubpart of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation"
                            Submission -> "Submission"
                        in
                          Html.map ReplyMsg <|
                          Form.viewWith title (ReplyForm.view msg) model.previewMessage

                  ReplySubpartId id ->
                    case idMapLookup id model.inbox.replySubpart of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation Reply"
                            Submission -> "Submission Reply"
                        in
                          Html.map ReplySeenMsg <|
                          Form.viewWith title (ReplySeenForm.view msg) model.previewReply

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
