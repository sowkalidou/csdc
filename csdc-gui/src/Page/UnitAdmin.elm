module Page.UnitAdmin exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxImageText as BoxImageText
import UI.BoxMessage as BoxMessage
import UI.BoxReply as BoxReply
import UI.Inbox as Inbox
import UI.Modal as Modal
import UI.Column as Column
import UI.Preview as Preview
import UI.PreviewImageText as PreviewImageText
import Notification exposing (Notification)
import Page as Page
import Types exposing (..)
import Form.MemberDelete as MemberDeleteForm
import Form.Reply as ReplyForm
import Form.ReplySeen as ReplySeenForm
import Form.UnitChair as UnitChairForm
import Form

import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedPerson (Id Person)
  | SelectedInbox Inbox.InboxId

type alias Model =
  { inbox : Inbox
  , selected : Maybe Selected
  , notification : Notification
  , memberSelected : Maybe (Id Member)
  , memberDelete : MemberDeleteForm.Model
  , formReply : ReplyForm.Model
  , replySeenForm : ReplySeenForm.Model
  , unitChairSelected : Maybe (Id Person)
  , unitChairForm : UnitChairForm.Model
  }

initial : Model
initial =
  { inbox = emptyInbox
  , selected = Nothing
  , notification = Notification.Empty
  , memberSelected = Nothing
  , memberDelete = MemberDeleteForm.initial
  , formReply = ReplyForm.initial
  , replySeenForm = ReplySeenForm.initial
  , unitChairSelected = Nothing
  , unitChairForm = UnitChairForm.initial
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map UnitInbox <| API.unitInbox id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = UnitInbox (API.Response Inbox)
  | SetSelectedMember (Maybe (Id Member))
  | MemberDeleteMsg MemberDeleteForm.Msg
  | SetSelectedUnitChair (Maybe (Id Person))
  | UnitChairMsg UnitChairForm.Msg
  | SetSelected Selected
  | ReplyMsg ReplyForm.Msg
  | ReplySeenMsg ReplySeenForm.Msg
  | ViewSelected (Id Person)
  | Reset
  | CloseModal

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
  let
    onSuccess = Notification.withResponse pageInfo Reset model
    reload = Page.goTo pageInfo <| Page.Unit Page.UnitAdmin info.id
  in
  case msg of
    SetSelectedMember member ->
      ( { model | memberSelected = member }
      , Cmd.none
      )

    SetSelectedUnitChair person ->
      ( { model | unitChairSelected = person }
      , Cmd.none
      )

    SetSelected selected ->
      ( { model | selected = Just selected }
      , Cmd.none
      )

    ViewSelected id ->
      ( model
      , Page.goTo pageInfo (Page.Person id)
      )

    MemberDeleteMsg memberMsg ->
      case model.memberSelected of
        Just member ->
          let
            config =
              { member = member
              , finish = reload
              , pageInfo = pageInfo
              }
            (memberDelete, cmd) = MemberDeleteForm.updateWith config memberMsg model.memberDelete
          in
            ( { model
              | memberDelete = memberDelete
              , memberSelected = if Form.isFinished memberMsg then Nothing else model.memberSelected
              }
            , Cmd.map MemberDeleteMsg cmd
            )

        Nothing -> (model, Cmd.none)

    UnitChairMsg chairMsg ->
      case model.unitChairSelected of
        Just person ->
          let
            config =
              { unit = info.id
              , person = person
              , finish = Page.goTo pageInfo <| Page.Unit Page.UnitInfo info.id
              , pageInfo = pageInfo
              }
            (unitChairForm, cmd) = UnitChairForm.updateWith config chairMsg model.unitChairForm
          in
            ( { model
              | unitChairForm = unitChairForm
              , unitChairSelected = if Form.isFinished chairMsg then Nothing else model.unitChairSelected
              }
            , Cmd.map UnitChairMsg cmd
            )

        Nothing -> (model, Cmd.none)

    ReplyMsg preMsg ->
      case model.selected of
        Just (SelectedInbox (Inbox.MessageMemberId id)) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplyMember { rtype = rtype, text = reason, message = id }
              , finish = reload
              , pageInfo = pageInfo
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

        Just (SelectedInbox (Inbox.MessageSubpartId id)) ->
          let
            config =
              { request = \(rtype, reason) ->
                  API.sendReplySubpart { rtype = rtype, text = reason, message = id }
              , finish = reload
              , pageInfo = pageInfo
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
        Just (SelectedInbox (Inbox.ReplyMemberId id)) ->
          let
            config =
              { request = API.viewReplyMember id
              , finish = reload
              , pageInfo = pageInfo
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

        Just (SelectedInbox (Inbox.ReplySubpartId id)) ->
          let
            config =
              { request = API.viewReplySubpart id
              , finish = reload
              , pageInfo = pageInfo
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
view info model =
  if not info.isAdmin
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
              [ Column.view "Members Admin" [] (viewPersons info.unit.chair info.members) ]
          , Html.div
              [ Html.Attributes.class "column is-half" ]
              [ Column.view "Inbox" [] <|
                List.map (Html.map (SetSelected << SelectedInbox)) <|
                Inbox.view model.inbox
              ]
          ]

      , Modal.viewMaybe model.selected CloseModal <| \selected ->
        case selected of
          SelectedPerson id ->
            case lookupById id info.members of
              Nothing ->
                Html.div [] [ Html.text "Loading..." ]
              Just unitMember ->
                PreviewImageText.view unitMember.person <|
                ViewSelected unitMember.id

          SelectedInbox inboxId ->
            case inboxId of
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

      , Modal.viewMaybe model.memberSelected (SetSelectedMember Nothing) <| \_ ->
          Html.map MemberDeleteMsg <|
          Form.viewWith "Remove From Unit" (MemberDeleteForm.view MemberDeleteForm.Unit) model.memberDelete

      , Modal.viewMaybe model.unitChairSelected (SetSelectedUnitChair Nothing) <| \_ ->
          Html.map UnitChairMsg <|
          Form.viewWith "Change Unit Chair" UnitChairForm.view model.unitChairForm

      ] ++
      Notification.view model.notification

viewPersons : Id Person -> List UnitMember -> List (Html Msg)
viewPersons chair members =
  let
    toBox member =
      BoxImageText.view
        False
        [ { label = "Remove from unit"
          , message = SetSelectedMember (Just member.member)
          }
        , { label = "Nominate chair"
          , message = SetSelectedUnitChair (Just chair)
          }
        ]
        (SetSelected (SelectedPerson member.id))
        member.person
  in
    List.map toBox <| List.filter (\member -> member.id /= chair) members
