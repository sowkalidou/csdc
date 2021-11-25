module CSDC.View.UnitAdmin exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.Input as Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.View.MessagePreview as MessagePreview
import CSDC.View.PersonPreview as PersonPreview
import CSDC.View.ReplyPreview as ReplyPreview
import CSDC.View.UnitPreview as UnitPreview

import Html exposing (Html)
import Html.Attributes
import Html.Events
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
  , previewMessage : MessagePreview.Model
  , previewReply : ReplyPreview.Model
  }

initial : Model
initial =
  { unit = Nothing
  , inbox = emptyInbox
  , panelMember = Panel.initial "Member Messages"
  , panelSubpart = Panel.initial "Subpart Messages"
  , selected = SelectedNothing
  , notification = Notification.Empty
  , previewMessage = MessagePreview.initial
  , previewReply = ReplyPreview.initial
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
  | MessagePreviewMsg MessagePreview.Msg
  | ReplyPreviewMsg ReplyPreview.Msg
  | Reset
  | CloseModal

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

    MessagePreviewMsg preMsg ->
      case model.selected of
        SelectedMember (MemberMessage mid) ->
          let
            (previewMessage, cmd) = MessagePreview.updateMember mid preMsg model.previewMessage
          in
            ( { model | previewMessage = previewMessage }
            , Cmd.map MessagePreviewMsg cmd
            )
        SelectedSubpart (SubpartMessage mid) ->
          let
            (previewMessage, cmd) = MessagePreview.updateSubpart mid preMsg model.previewMessage
          in
            ( { model | previewMessage = previewMessage }
            , Cmd.map MessagePreviewMsg cmd
            )
        _ ->
          (model, Cmd.none)

    ReplyPreviewMsg preMsg ->
      case model.selected of
        SelectedMember (MemberReply mid) ->
          let
            (previewReply, cmd) = ReplyPreview.updateMember mid preMsg model.previewReply
          in
            ( { model | previewReply = previewReply }
            , Cmd.map ReplyPreviewMsg cmd
            )
        SelectedSubpart (SubpartReply mid) ->
          let
            (previewReply, cmd) = ReplyPreview.updateSubpart mid preMsg model.previewReply
          in
            ( { model | previewReply = previewReply }
            , Cmd.map ReplyPreviewMsg cmd
            )
        _ ->
          (model, Cmd.none)

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
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
        API.UnitInbox uid result -> onSuccess result <| \inbox ->
          let
            memberPairs =
              let
                fmm (id, m) =
                  { index = MemberMessage id
                  , title = ""
                  , description = m.text
                  }
                frm (id, r) =
                  { index = MemberReply id
                  , title = ""
                  , description = r.text
                  }
              in
                List.map fmm (idMapToList inbox.messageMember) ++
                List.map frm (idMapToList inbox.replyMember)

            subpartPairs =
              let
                fms (id, m) =
                  { index = SubpartMessage id
                  , title = ""
                  , description = m.text
                  }
                frs (id, r) =
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

canEdit : Maybe PersonInfo -> UnitInfo -> Bool
canEdit muser unitInfo =
  case muser of
    Nothing -> False
    Just person -> person.id == unitInfo.unit.chair

view : Maybe PersonInfo -> Model -> List (Html Msg)
view mid model =
  case model.unit of
    Nothing ->
      [ Html.text "Loading..."
      ] ++ Notification.view model.notification

    Just unit ->
      if not (canEdit mid unit)
        then
          [ Html.text "You cannot edit this unit."
          ]
        else
          [ Html.h1
              [ Html.Attributes.class "title" ]
              [ Html.text "Unit Admin" ]
          , Html.div
              [ Html.Attributes.class "columns"
              , Html.Attributes.style "height" "100%"
              ]
              [ Html.div
                  [ Html.Attributes.class "column is-half" ]
                  [ Html.map PanelMemberMsg <| Panel.view model.panelMember ]
              , Html.div
                  [ Html.Attributes.class "column is-half" ]
                  [ Html.map PanelSubpartMsg <| Panel.view model.panelSubpart ]
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

                  SelectedMember memberId ->
                    Html.div [] <|
                      case memberId of
                        MemberMessage rid ->
                          case idMapLookup rid model.inbox.messageMember of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map MessagePreviewMsg <|
                              MessagePreview.view msg model.previewMessage

                        MemberReply rid ->
                          case idMapLookup rid model.inbox.replyMember of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map ReplyPreviewMsg (ReplyPreview.view msg)

                  SelectedSubpart subpartId ->
                    Html.div [] <|
                      case subpartId of
                        SubpartMessage rid ->
                          case idMapLookup rid model.inbox.messageSubpart of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map MessagePreviewMsg <|
                              MessagePreview.view msg model.previewMessage

                        SubpartReply rid ->
                          case idMapLookup rid model.inbox.replySubpart of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map ReplyPreviewMsg (ReplyPreview.view msg)
          ] ++
          Notification.view model.notification
