module CSDC.View.UnitAdmin exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.UI.BoxMessage as BoxMessage
import CSDC.UI.BoxReply as BoxReply
import CSDC.UI.Modal as Modal
import CSDC.UI.Column as Column
import CSDC.UI.Preview as Preview
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.Form.Reply as ReplyForm
import CSDC.Form.ReplySeen as ReplySeenForm
import Form

import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Model

type MemberId
  = MessageMemberId (Id (Message NewMember))
  | ReplyMemberId (Id (Reply NewMember))

type SubpartId
  = MessageSubpartId (Id (Message NewSubpart))
  | ReplySubpartId (Id (Reply NewSubpart))

type Selected
  = SelectedMember MemberId
  | SelectedSubpart SubpartId

type alias Model =
  { inbox : Inbox
  , selected : Maybe Selected
  , notification : Notification
  , previewMessage : ReplyForm.Model
  , previewReply : ReplySeenForm.Model
  }

initial : Model
initial =
  { inbox = emptyInbox
  , selected = Nothing
  , notification = Notification.Empty
  , previewMessage = ReplyForm.initial
  , previewReply = ReplySeenForm.initial
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map UnitInbox <| API.unitInbox id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = UnitInbox (API.Response Inbox)
  | ReplyMsg ReplyForm.Msg
  | ReplySeenMsg ReplySeenForm.Msg
  | SetSelected Selected
  | Reset
  | CloseModal

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
  let
    onSuccess = Notification.withResponse Reset model
    reload = Page.goTo pageInfo <| Page.Unit Page.UnitAdmin info.id
  in
  case msg of
    SetSelected selected ->
      ( { model | selected = Just selected }
      , Cmd.none
      )

    ReplyMsg preMsg ->
      case model.selected of
        Just (SelectedMember (MessageMemberId id)) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplyMember { rtype = rtype, text = reason, message = id }
              , finish = reload
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

        Just (SelectedSubpart (MessageSubpartId id)) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplySubpart { rtype = rtype, text = reason, message = id }
              , finish = reload
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

    ReplySeenMsg preMsg ->
      case model.selected of
        Just (SelectedMember (ReplyMemberId id)) ->
          let
            config =
              { request = API.viewReplyMember id
              , finish = reload
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

        Just (SelectedSubpart (ReplySubpartId id)) ->
          let
            config =
              { request = API.viewReplySubpart id
              , finish = reload
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

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    CloseModal ->
      ( { model | selected = Nothing }
      , Cmd.none
      )

    UnitInbox result -> onSuccess result <| \inbox ->
      ( { model | inbox = inbox }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : UnitInfo -> Model -> List (Html Msg)
view unit model =
  if not unit.isAdmin
    then
      [ Html.text "You cannot edit this unit."
      ]
    else
      [ Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-half" ]
              [ Column.view "Member Messages" [] (viewInboxMember model.inbox) ]
          , Html.div
              [ Html.Attributes.class "column is-half" ]
              [ Column.view "Subpart Messages" [] (viewInboxSubpart model.inbox) ]
          ]

      , Modal.viewMaybe model.selected CloseModal <| \selected ->
          case selected of
            SelectedMember memberId ->
              Html.div [] <|
                case memberId of
                  MessageMemberId rid ->
                    case lookupById rid model.inbox.messageMember of
                      Nothing ->
                        [ Html.text "Error." ]
                      Just msg ->
                        List.singleton <|
                        Html.map ReplyMsg <|
                        Preview.make <|
                        ReplyForm.view msg model.previewMessage

                  ReplyMemberId rid ->
                    case lookupById rid model.inbox.replyMember of
                      Nothing ->
                        [ Html.text "Error." ]
                      Just msg ->
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation Reply"
                            Submission -> "Submission Reply"
                        in
                          List.singleton <|
                          Html.map ReplySeenMsg <|
                          Form.viewWith title (ReplySeenForm.view msg) model.previewReply

            SelectedSubpart subpartId ->
              Html.div [] <|
                case subpartId of
                  MessageSubpartId rid ->
                    case lookupById rid model.inbox.messageSubpart of
                      Nothing ->
                        [ Html.text "Error." ]
                      Just msg ->
                        List.singleton <|
                        Html.map ReplyMsg <|
                        Preview.make <|
                        ReplyForm.view msg model.previewMessage

                  ReplySubpartId rid ->
                    case lookupById rid model.inbox.replySubpart of
                      Nothing ->
                        [ Html.text "Error." ]
                      Just msg ->
                        let
                          title = case msg.mtype of
                            Invitation -> "Invitation Reply"
                            Submission -> "Submission Reply"
                        in
                          List.singleton <|
                          Html.map ReplySeenMsg <|
                          Form.viewWith title (ReplySeenForm.view msg) model.previewReply
      ] ++
      Notification.view model.notification

viewInboxMember : Inbox -> List (Html Msg)
viewInboxMember inbox =
  let
    toMsg iid =
      SetSelected (SelectedMember iid)
    toBoxMessageMember =
      Html.map (toMsg << MessageMemberId) << BoxMessage.view False
    toBoxReplyMember =
      Html.map (toMsg << ReplyMemberId) << BoxReply.view False
  in
    List.map toBoxMessageMember inbox.messageMember ++
    List.map toBoxReplyMember inbox.replyMember

viewInboxSubpart : Inbox -> List (Html Msg)
viewInboxSubpart inbox =
  let
    toMsg iid =
      SetSelected (SelectedSubpart iid)
    toBoxMessageSubpart =
      Html.map (toMsg << MessageSubpartId) << BoxMessage.view False
    toBoxReplySubpart =
      Html.map (toMsg << ReplySubpartId) << BoxReply.view False
  in
    List.map toBoxMessageSubpart inbox.messageSubpart ++
    List.map toBoxReplySubpart inbox.replySubpart

