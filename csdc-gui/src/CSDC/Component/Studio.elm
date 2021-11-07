module CSDC.Component.Studio exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  , Selected (..)
  , ViewSelected (..)
  )

import CSDC.API as API
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewMessage as PreviewMessage
import CSDC.Component.PreviewReply as PreviewReply
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Component.Progress as Progress
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)

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
  }

initial : Model
initial =
  { info = Nothing
  , panelUnits = Panel.initial "Units"
  , panelMessages = Panel.initial "Messages"
  , notification = Notification.Empty
  , inbox = emptyInbox
  , selected = SelectedNothing
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

type ViewSelected
  = ViewSelectedUnit (Id Unit)
  | ViewSelectedInbox InboxId MessageType ReplyType

type Msg
  = APIMsg API.Msg
  | UnitsMsg (Panel.Msg (Id Member))
  | MessagesMsg (Panel.Msg InboxId)
  | PreviewMessageMemberMsg (PreviewMessage.Msg Member)
  | PreviewMessageSubpartMsg (PreviewMessage.Msg Subpart)
  | PreviewReplyMemberMsg (PreviewReply.Msg Member)
  | PreviewReplySubpartMsg (PreviewReply.Msg Subpart)
  | CreateUnit
  | View ViewSelected
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

    View selected ->
      case selected of
        ViewSelectedUnit id ->
          ( { model | selected = SelectedNothing }
          , Page.goTo pageInfo (Page.ViewUnit id)
          )

        ViewSelectedInbox inboxId mtype rtype ->
          case inboxId of
            MessageMemberId id ->
              let
                reply = Reply
                  { rtype = rtype
                  , mtype = mtype
                  , text = "Reply"
                  , status = NotSeen
                  , id = id
                  }
              in
                ( { model | selected = SelectedNothing }
                , Cmd.map APIMsg <| API.sendReplyMember reply
                )

            _ ->
             (model, Cmd.none)

    CreateUnit ->
      ( model
      , case model.info of
          Nothing -> Cmd.none
          Just info -> Cmd.map APIMsg <| API.createUnit info.id
      )

    PreviewMessageMemberMsg (PreviewMessage.Reply { message, messageType }) ->
      ( model
      , Page.goTo pageInfo (Page.ReplyMember message messageType)
      )

    PreviewMessageSubpartMsg (PreviewMessage.Reply { message, messageType }) ->
      ( model
      , Page.goTo pageInfo (Page.ReplySubpart message messageType)
      )

    PreviewReplyMemberMsg (PreviewReply.MarkAsSeen id) ->
      ( { model | selected = SelectedNothing }
      , Cmd.map APIMsg <|
        API.viewReplyMember id
      )

    PreviewReplySubpartMsg (PreviewReply.MarkAsSeen id) ->
      ( { model | selected = SelectedNothing }
      , Cmd.map APIMsg <|
        API.viewReplySubpart id
      )

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

        API.CreateUnit result -> onSuccess result <| \member ->
            let
              uid = getMemberUnit member.value
            in
              ( model
              , Page.goTo pageInfo (Page.ViewUnit uid)
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

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
              [ Html.Attributes.class "column" ]
              [ Html.div
                  []
                  [ Html.text info.person.name ]
              , Html.div
                  []
                  [ Html.strong [] [ Html.text "ORCID ID: " ]
                  , Html.a
                      [ Html.Attributes.href ("https://orcid.org/" ++ info.person.orcid)
                      , Html.Attributes.target "_blank"
                      ]
                      [ Html.text info.person.orcid ]
                  ]
              , Html.div
                  []
                  [ Html.text info.person.description ]
              , Html.div
                  []
                  [ Html.button
                      [ Html.Attributes.class "button is-primary is-pulled-right"
                      , Html.Events.onClick CreateUnit
                      ]
                      [ Html.text "Create New Unit"
                      ]
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column" ]
              [ Html.map UnitsMsg <| Panel.view model.panelUnits ]
          , Html.div
              [ Html.Attributes.class "column" ]
              [ Html.map MessagesMsg <| Panel.view model.panelMessages ]
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
                    PreviewUnit.view unit.value <|
                    View (ViewSelectedUnit unit.id)

{-
              SelectedInbox inboxId ->
                row
                  [ height <| fillPortion 1
                  , width fill
                  ] <|
                  case inboxId of
                    MessageMemberId id ->
                      case idMapLookup id model.inbox.messageMember of
                        Nothing ->
                          [ text "Error." ]
                        Just msg ->
                          List.map (map PreviewMessageMemberMsg) <|
                          PreviewMessage.view id msg

                    ReplyMemberId id ->
                      case idMapLookup id model.inbox.replyMember of
                        Nothing ->
                          [ text "Error." ]
                        Just msg ->
                          List.map (map PreviewReplyMemberMsg) <|
                          PreviewReply.view id msg

                    MessageSubpartId id ->
                      case idMapLookup id model.inbox.messageSubpart of
                        Nothing ->
                          [ text "Error." ]
                        Just msg ->
                          List.map (map PreviewMessageSubpartMsg) <|
                          PreviewMessage.view id msg

                    ReplySubpartId id ->
                      case idMapLookup id model.inbox.replySubpart of
                        Nothing ->
                          [ text "Error." ]
                        Just msg ->
                          List.map (map PreviewReplySubpartMsg) <|
                          PreviewReply.view id msg
-}
              _ -> Html.div [] []

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
    fmm (id, MessageInfo m) =
      { index = MessageMemberId id
      , title = ""
      , description = m.text
      }
    frm (id, ReplyInfo r) =
      { index = ReplyMemberId id
      , title = ""
      , description = r.text
      }
    fms (id, MessageInfo m) =
      { index = MessageSubpartId id
      , title = ""
      , description = m.text
      }
    frs (id, ReplyInfo r) =
      { index = ReplySubpartId id
      , title = ""
      , description = r.text
      }
  in
    List.map fmm (idMapToList inbox.messageMember) ++
    List.map frm (idMapToList inbox.replyMember) ++
    List.map fms (idMapToList inbox.messageSubpart) ++
    List.map frs (idMapToList inbox.replySubpart)
