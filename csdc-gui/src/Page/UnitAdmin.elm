module Page.UnitAdmin exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxMessage as BoxMessage
import UI.BoxReply as BoxReply
import UI.Inbox as Inbox
import UI.Modal as Modal
import UI.Column as Column
import UI.Preview as Preview
import Notification exposing (Notification)
import Page as Page
import Types exposing (..)
import Form.Reply as ReplyForm
import Form.ReplySeen as ReplySeenForm
import Form

import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { inbox : Inbox
  , selected : Maybe Inbox.InboxId
  , notification : Notification
  , formReply : ReplyForm.Model
  , replySeenForm : ReplySeenForm.Model
  }

initial : Model
initial =
  { inbox = emptyInbox
  , selected = Nothing
  , notification = Notification.Empty
  , formReply = ReplyForm.initial
  , replySeenForm = ReplySeenForm.initial
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
  | SetSelected Inbox.InboxId
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
        Just (Inbox.MessageMemberId id) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplyMember { rtype = rtype, text = reason, message = id }
              , finish = reload
              }

            (formReply, cmd) = ReplyForm.updateWith config preMsg model.formReply
          in
            ( { model
              | formReply = formReply
              , selected =
                  if Form.isFinished preMsg then Nothing else model.selected
              }
            , Cmd.map ReplyMsg cmd
            )

        Just (Inbox.MessageSubpartId id) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplySubpart { rtype = rtype, text = reason, message = id }
              , finish = reload
              }

            (formReply, cmd) = ReplyForm.updateWith config preMsg model.formReply
          in
            ( { model
              | formReply = formReply
              , selected =
                  if Form.isFinished preMsg then Nothing else model.selected
              }
            , Cmd.map ReplyMsg cmd
            )

        _ ->
          (model, Cmd.none)

    ReplySeenMsg preMsg ->
      case model.selected of
        Just (Inbox.ReplyMemberId id) ->
          let
            config =
              { request = API.viewReplyMember id
              , finish = reload
              }

            (replySeenForm, cmd) = ReplySeenForm.updateWith config preMsg model.replySeenForm
          in
            ( { model
              | replySeenForm = replySeenForm
              , selected =
                  if Form.isFinished preMsg then Nothing else model.selected
              }
            , Cmd.map ReplySeenMsg cmd
            )

        Just (Inbox.ReplySubpartId id) ->
          let
            config =
              { request = API.viewReplySubpart id
              , finish = reload
              }

            (replySeenForm, cmd) = ReplySeenForm.updateWith config preMsg model.replySeenForm
          in
            ( { model
              | replySeenForm = replySeenForm
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
              [ Column.view "Members Admin" [] [] ]
          , Html.div
              [ Html.Attributes.class "column is-half" ]
              [ Column.view "Inbox" [] <|
                List.map (Html.map SetSelected) <|
                Inbox.view model.inbox
              ]
          ]

      , Modal.viewMaybe model.selected CloseModal <| \selected ->
        case selected of
          Inbox.MessageMemberId rid ->
            case lookupById rid model.inbox.messageMember of
              Nothing ->
                Html.text "Error."
              Just msg ->
                Html.map ReplyMsg <|
                Preview.make <|
                ReplyForm.view msg model.formReply

          Inbox.ReplyMemberId rid ->
            case lookupById rid model.inbox.replyMember of
              Nothing ->
                Html.text "Error."
              Just msg ->
                let
                  title = case msg.mtype of
                    Invitation -> "Invitation Reply"
                    Submission -> "Submission Reply"
                in
                  Html.map ReplySeenMsg <|
                  Form.viewWith title (ReplySeenForm.view msg) model.replySeenForm

          Inbox.MessageSubpartId rid ->
            case lookupById rid model.inbox.messageSubpart of
              Nothing ->
                Html.text "Error."
              Just msg ->
                Html.map ReplyMsg <|
                Preview.make <|
                ReplyForm.view msg model.formReply

          Inbox.ReplySubpartId rid ->
            case lookupById rid model.inbox.replySubpart of
              Nothing ->
                Html.text "Error."
              Just msg ->
                let
                  title = case msg.mtype of
                    Invitation -> "Invitation Reply"
                    Submission -> "Submission Reply"
                in
                  Html.map ReplySeenMsg <|
                  Form.viewWith title (ReplySeenForm.view msg) model.replySeenForm
      ] ++
      Notification.view model.notification
