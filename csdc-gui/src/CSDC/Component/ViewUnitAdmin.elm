module CSDC.Component.ViewUnitAdmin exposing
  ( Model
  , setup
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
import CSDC.Page as Page
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
  { unit : Maybe UnitInfo
  , inbox : Inbox
  , panelMember : Panel.Model MemberId
  , panelSubpart : Panel.Model SubpartId
  , selected : Selected
  , notification : Notification
  }

initial : Model
initial =
  { unit = Nothing
  , inbox = emptyInbox
  , panelMember = Panel.initial "Member Messages"
  , panelSubpart = Panel.initial "Subpart Messages"
  , selected = SelectedNothing
  , notification = Notification.Empty
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map APIMsg <| API.getUnitInfo id
  , Cmd.map APIMsg <| API.unitInbox id
  ]

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
  | Reset

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
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

    PreviewMessageMemberMsg (PreviewMessage.Reply r) ->
          ( model
          , Page.goTo pageInfo (Page.ReplyMember r.message r.messageType)
          )

    PreviewMessageSubpartMsg (PreviewMessage.Reply r) ->
          ( model
          , Page.goTo pageInfo (Page.ReplySubpart r.message r.messageType)
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

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    APIMsg apimsg ->
      let
        onSuccess = Notification.withResponse Reset model
      in
      case apimsg of
        API.UnitInbox uid result -> onSuccess result <| \inbox ->
          let
            memberPairs =
              let
                fmm (id, MessageInfo m) =
                  { index = MemberMessage id
                  , title = ""
                  , description = m.text
                  }
                frm (id, ReplyInfo r) =
                  { index = MemberReply id
                  , title = ""
                  , description = r.text
                  }
              in
                List.map fmm (idMapToList inbox.messageMember) ++
                List.map frm (idMapToList inbox.replyMember)

            subpartPairs =
              let
                fms (id, MessageInfo m) =
                  { index = SubpartMessage id
                  , title = ""
                  , description = m.text
                  }
                frs (id, ReplyInfo r) =
                  { index = SubpartReply id
                  , title = ""
                  , description = r.text
                  }
              in
                List.map fms (idMapToList inbox.messageSubpart) ++
                List.map frs (idMapToList inbox.replySubpart)

            panelMember =
              Panel.update (Panel.SetItems memberPairs) model.panelMember

            panelSubpart =
              Panel.update (Panel.SetItems subpartPairs) model.panelSubpart

          in
            ( { model
              | panelMember = panelMember
              , panelSubpart = panelSubpart
              , inbox = inbox
              }
            , Cmd.none
            )

        API.ViewReplyMember result -> onSuccess result <| \_ ->
          case model.unit of
            Nothing -> (model, Cmd.none)
            Just unit -> (model, setup unit.id)

        API.GetUnitInfo result -> onSuccess result <| \info ->
          ( { model | unit = Just info }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

canEdit : Maybe (User PersonInfo) -> UnitInfo -> Bool
canEdit muser unitInfo =
  case muser of
    Nothing -> False
    Just Admin -> True
    Just (User person) -> person.id == unitInfo.unit.chair

view : Maybe (User PersonInfo) -> Model -> List (Element Msg)
view mid model =
  case model.unit of
    Nothing ->
      [ text "Loading..."
      ] ++
      List.map html (Notification.view model.notification)

    Just unit ->
      if not (canEdit mid unit)
        then
          [ text "You cannot edit this unit."
          ]
        else
      [ row
          [ Font.bold, Font.size 30 ]
          [ text "Unit Admin" ]
      , row
          [ height <| fillPortion 1
          , width fill
          , spacing 10
          ]
          [ map PanelMemberMsg <| Element.html (Panel.view model.panelMember)
          , map PanelSubpartMsg <| Element.html (Panel.view model.panelSubpart)
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
      List.map html (Notification.view model.notification)
