module View.Studio exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  , Selected (..)
  )

import API as API
import UI.BoxMessage as BoxMessage
import UI.BoxReply as BoxReply
import UI.BoxImageText as BoxImageText
import UI.Column as Column
import UI.DotMenu as DotMenu
import UI.Modal as Modal
import UI.Progress as Progress
import UI.PreviewImageText as PreviewImageText
import Form.Unit as UnitForm
import Form.Person as PersonForm
import Form.Image as ImageForm
import Form.ReplySeen as ReplySeenForm
import Form.Reply as ReplyForm
import Notification as Notification exposing (Notification)
import Page as Page
import Types exposing (..)
import Form

import Html exposing (Html)
import Html.Attributes
import Markdown

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedUnit (Id Unit)
  | SelectedInbox InboxId

type alias Model =
  { info : Maybe PersonInfo
  , notification : Notification
  , inbox : Inbox
  , selected : Maybe Selected
  , unitCreate : UnitForm.Model
  , unitCreateOpen : Bool
  , personEdit : PersonForm.Model
  , personEditOpen : Bool
  , personImage : ImageForm.Model
  , personImageOpen : Bool
  , previewMessage : ReplyForm.Model
  , previewReply : ReplySeenForm.Model
  }

initial : Model
initial =
  { info = Nothing
  , notification = Notification.Empty
  , inbox = emptyInbox
  , selected = Nothing
  , unitCreate = UnitForm.initial
  , unitCreateOpen = False
  , personEdit = PersonForm.initial
  , personEditOpen = False
  , personImage = ImageForm.initial
  , personImageOpen = False
  , previewMessage = ReplyForm.initial
  , previewReply = ReplySeenForm.initial
  }

setup : Cmd Msg
setup =
  Cmd.batch
    [ Cmd.map GetUserInfo <| API.getUserInfo
    , Cmd.map GetUserInbox <| API.getUserInbox
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUserInfo (API.Response PersonInfo)
  | GetUserInbox (API.Response Inbox)
  | ReplyMsg ReplyForm.Msg
  | ReplySeenMsg ReplySeenForm.Msg
  | UnitCreateMsg (UnitForm.Msg (Id Unit))
  | UnitCreateOpen
  | UnitCreateClose
  | PersonEditMsg PersonForm.Msg
  | PersonEditOpen
  | PersonEditClose
  | ImageMsg ImageForm.Msg
  | ImageOpen
  | ImageClose
  | SetSelected Selected
  | View (Id Unit)
  | CloseModal
  | Reset

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse Reset model
  in
  case msg of
    Reset ->
      ({ model | notification = Notification.Empty }, Cmd.none)

    SetSelected selected ->
      ( { model | selected = Just selected }
      , Cmd.none
      )

    View id ->
      ( { model | selected = Nothing }
      , Page.goTo pageInfo (Page.Unit Page.UnitInfo id)
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
      let
        config =
          { request = API.createUnit
          , finish = \id -> Page.goTo pageInfo (Page.Unit Page.UnitInfo id)
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

    ImageOpen ->
      ( { model
        | personImageOpen = True
        }
      , Cmd.map ImageMsg <| ImageForm.setup Nothing
      )

    ImageClose ->
      ( { model | personImageOpen = False }
      , Cmd.none
      )

    ImageMsg personMsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just person ->
          let
            config =
              { request = API.updatePersonImage person.id
              , finish = Page.reload
              }
            (personImage, cmd) = ImageForm.updateWith config personMsg model.personImage
          in
            ( { model
              | personImage = personImage
              , personImageOpen = not (Form.isFinished personMsg)
              }
            , Cmd.map ImageMsg cmd
            )

    ReplyMsg preMsg ->
      case model.selected of
        Just (SelectedInbox inboxId) ->
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
                      if Form.isFinished preMsg then Nothing else model.selected
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
                      if Form.isFinished preMsg then Nothing else model.selected
                  }
                , Cmd.map ReplyMsg cmd
                )

            _ ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    ReplySeenMsg preMsg ->
      case model.selected of
        Just (SelectedInbox inboxId) ->
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
                      if Form.isFinished preMsg then Nothing else model.selected
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
                      if Form.isFinished preMsg then Nothing else model.selected
                  }
                , Cmd.map ReplySeenMsg cmd
                )

            _ ->
              (model, Cmd.none)
        _ ->
          (model, Cmd.none)

    CloseModal ->
      ( { model | selected = Nothing }
      , Cmd.none
      )

    GetUserInfo result -> onSuccess result <| \info ->
      ( { model | info = Just info }
      , Cmd.none
      )

    GetUserInbox result -> onSuccess result <| \inbox ->
      ( { model | inbox = inbox }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

dotMenu : Html Msg
dotMenu = DotMenu.make
  [ { label = "Edit Profile"
    , message = PersonEditOpen
    }
  , { label = "Change Profile Photo"
    , message = ImageOpen
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
              [ Column.view "Information" [dotMenu]
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
                                  [ Html.Attributes.src <| filePath info.person.image
                                  , Html.Attributes.style "border-radius" "10%"
                                  , Html.Attributes.alt "Profile Photo"
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
                  , Markdown.toHtml
                      [ Html.Attributes.class "content"]
                      (info.person.description)
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view "Units" [] (viewUnits info.members) ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view "Inbox" [] (viewInbox model.inbox) ]
          ]

      , Modal.view model.personEditOpen PersonEditClose <|
          Html.map PersonEditMsg <|
          Form.viewWith "Edit Profile" PersonForm.view model.personEdit

      , Modal.view model.personImageOpen ImageClose <|
          Html.map ImageMsg <|
          Form.viewWith "Profile Photo" ImageForm.view model.personImage

      , Modal.view model.unitCreateOpen UnitCreateClose <|
          Html.map UnitCreateMsg <|
          Form.viewWith "Create Unit" UnitForm.view model.unitCreate

      , Modal.viewMaybe model.selected CloseModal <| \selected ->
          case selected of
            SelectedUnit id ->
              case lookupById id info.members of
                Nothing ->
                  Html.text "Error."
                Just personMember ->
                  PreviewImageText.view personMember.unit (View personMember.id)

            SelectedInbox inboxId ->
              case inboxId of
                ReplyMemberId id ->
                  case lookupById id model.inbox.replyMember of
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
                  case lookupById id model.inbox.messageMember of
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
                  case lookupById id model.inbox.messageSubpart of
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
                  case lookupById id model.inbox.replySubpart of
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
  = MessageMemberId (Id (Message NewMember))
  | ReplyMemberId (Id (Reply NewMember))
  | MessageSubpartId (Id (Message NewSubpart))
  | ReplySubpartId (Id (Reply NewSubpart))

viewInbox : Inbox -> List (Html Msg)
viewInbox inbox =
  let
    toMsg iid =
      SetSelected (SelectedInbox iid)
    toBoxMessageMember =
      Html.map (toMsg << MessageMemberId) << BoxMessage.view False
    toBoxReplyMember =
      Html.map (toMsg << ReplyMemberId) << BoxReply.view False
    toBoxMessageSubpart =
      Html.map (toMsg << MessageSubpartId) << BoxMessage.view False
    toBoxReplySubpart =
      Html.map (toMsg << ReplySubpartId) << BoxReply.view False
  in
    List.map toBoxMessageMember inbox.messageMember ++
    List.map toBoxReplyMember inbox.replyMember ++
    List.map toBoxMessageSubpart inbox.messageSubpart ++
    List.map toBoxReplySubpart inbox.replySubpart

viewUnits : List PersonMember -> List (Html Msg)
viewUnits members =
  let
    toBox member =
      Html.map (SetSelected << SelectedUnit) <|
      BoxImageText.view False member.id member.unit
  in
    List.map toBox members
