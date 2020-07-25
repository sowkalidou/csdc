module CSDC.Component.ViewUnitAdmin exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Component.PreviewMessage as PreviewMessage
import CSDC.Component.PreviewPerson as PreviewPerson
import CSDC.Component.PreviewReply as PreviewReply
import CSDC.Component.PreviewUnit as PreviewUnit
import CSDC.Input exposing (..)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Types exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type MemberId
  = MemberMessage (Id (Message Member))
  | MemberReply (Id (Reply Member))

type SubpartId
  = SubpartMessage (Id (Message Subpart))
  | SubpartReply (Id (Reply Subpart))

type Selected
  = SelectedNothing
  | SelectedMember MemberId
  | SelectedSubpart SubpartId

type alias Model =
  { id : Maybe (Id Unit)
  , unit : Maybe Unit
  , inbox : Inbox
  , panelMember : Panel.Model MemberId
  , panelSubpart : Panel.Model SubpartId
  , selected : Selected
  , notification : Notification
  }

initial : Model
initial =
  { id = Nothing
  , unit = Nothing
  , inbox = emptyInbox
  , panelMember = Panel.initial "Member Messages"
  , panelSubpart = Panel.initial "Subpart Messages"
  , selected = SelectedNothing
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | PanelMemberMsg (Panel.Msg MemberId)
  | PanelSubpartMsg (Panel.Msg SubpartId)
  | PreviewMessageMemberMsg (PreviewMessage.Msg Member)
  | PreviewMessageSubpartMsg (PreviewMessage.Msg Subpart)
  | PreviewReplyMemberMsg (PreviewReply.Msg Member)
  | PreviewReplySubpartMsg (PreviewReply.Msg Subpart)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PanelMemberMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelMember = Panel.update m model.panelMember
            , selected = SelectedMember id
            }
          , Cmd.none
          )

        _ ->
          ( { model
            | panelMember = Panel.update m model.panelMember
            }
          , Cmd.none
          )

    PanelSubpartMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelSubpart = Panel.update m model.panelSubpart
            , selected = SelectedSubpart id
            }
          , Cmd.none
          )

        _ ->
          ( { model
            | panelSubpart = Panel.update m model.panelSubpart
            }
          , Cmd.none
          )

    PreviewMessageMemberMsg _ -> (model, Cmd.none)

    PreviewMessageSubpartMsg _ -> (model, Cmd.none)

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

    APIMsg apimsg ->
      case apimsg of
        API.UnitInbox uid result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok inbox ->
              let
                memberPairs =
                  let
                    fmm (id, MessageInfo m) = (MemberMessage id, m.text)
                    frm (id, ReplyInfo r) = (MemberReply id, r.text)
                  in
                    List.map fmm (idMapToList inbox.messageMember) ++
                    List.map frm (idMapToList inbox.replyMember)

                subpartPairs =
                  let
                    fms (id, MessageInfo m) = (SubpartMessage id, m.text)
                    frs (id, ReplyInfo r) = (SubpartReply id, r.text)
                  in
                    List.map fms (idMapToList inbox.messageSubpart) ++
                    List.map frs (idMapToList inbox.replySubpart)

                panelMember =
                  Panel.update (Panel.SetItems memberPairs) model.panelMember

                panelSubpart =
                  Panel.update (Panel.SetItems subpartPairs) model.panelSubpart

              in
                ( { model
                  | id = Just uid
                  , panelMember = panelMember
                  , panelSubpart = panelSubpart
                  , inbox = inbox
                  }
                , Cmd.none
                )

        API.ViewReplyMember result ->
          case result of
            Err err ->
              ( { model | notification = Notification.HttpError err }
              , Cmd.none
              )

            Ok _ ->
              ( model -- XXX: RELOAD
              , Cmd.none
              )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

view : Maybe (User PersonInfo) -> Model -> List (Element Msg)
view mid model =
  case model.id of
    Nothing ->
      [ text "Loading..."
      ] ++
      Notification.view model.notification

    Just id ->
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Admin" ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map PanelMemberMsg <| Panel.view model.panelMember
          , map PanelSubpartMsg <| Panel.view model.panelSubpart
          ]
      , case model.selected of
          SelectedNothing ->
            row [] []

          SelectedMember memberId ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case memberId of
                MemberMessage rid ->
                  case idMapLookup rid model.inbox.messageMember of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      List.map (map PreviewMessageMemberMsg) <|
                      PreviewMessage.view rid msg

                MemberReply rid ->
                  case idMapLookup rid model.inbox.replyMember of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      List.map (map PreviewReplyMemberMsg) <|
                      PreviewReply.view rid msg

          SelectedSubpart subpartId ->
            row
              [ height <| fillPortion 1
              , width fill
              ] <|
              case subpartId of
                SubpartMessage rid ->
                  case idMapLookup rid model.inbox.messageSubpart of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      List.map (map PreviewMessageSubpartMsg) <|
                      PreviewMessage.view rid msg

                SubpartReply rid ->
                  case idMapLookup rid model.inbox.replySubpart of
                    Nothing ->
                      [ text "Error." ]
                    Just msg ->
                      List.map (map PreviewReplySubpartMsg) <|
                      PreviewReply.view rid msg

      ] ++
      Notification.view model.notification
