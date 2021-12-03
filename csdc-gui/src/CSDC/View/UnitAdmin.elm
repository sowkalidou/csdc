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
import CSDC.Component.Preview as Preview
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
  = MemberMessage (Id (Message NewMember))
  | MemberReply (Id (Reply NewMember))

type SubpartId
  = SubpartMessage (Id (Message NewSubpart))
  | SubpartReply (Id (Reply NewSubpart))

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
  , previewMessage : ReplyForm.Model
  , previewReply : ReplySeenForm.Model
  }

initial : Model
initial =
  { unit = Nothing
  , inbox = emptyInbox
  , panelMember = Panel.initial "Member Messages"
  , panelSubpart = Panel.initial "Subpart Messages"
  , selected = SelectedNothing
  , notification = Notification.Empty
  , previewMessage = ReplyForm.initial
  , previewReply = ReplySeenForm.initial
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map GetUnitInfo <| API.getUnitInfo id
  , Cmd.map UnitInbox <| API.unitInbox id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUnitInfo (API.Response UnitInfo)
  | UnitInbox (API.Response Inbox)
  | PanelMemberMsg (Panel.Msg MemberId)
  | PanelSubpartMsg (Panel.Msg SubpartId)
  | ReplyMsg ReplyForm.Msg
  | ReplySeenMsg ReplySeenForm.Msg
  | Reset
  | CloseModal

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse Reset model
    reload = case model.unit of
      Nothing -> Cmd.none
      Just info -> Page.goTo pageInfo <| Page.UnitAdmin info.id
  in
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

    ReplyMsg preMsg ->
      case model.selected of
        SelectedMember (MemberMessage id) ->
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
                  if Form.isFinished preMsg then SelectedNothing else model.selected
              }
            , Cmd.map ReplyMsg cmd
            )

        SelectedSubpart (SubpartMessage id) ->
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
                  if Form.isFinished preMsg then SelectedNothing else model.selected
              }
            , Cmd.map ReplyMsg cmd
            )

        _ ->
          (model, Cmd.none)

    ReplySeenMsg preMsg ->
      case model.selected of
        SelectedMember (MemberReply id) ->
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
                  if Form.isFinished preMsg then SelectedNothing else model.selected
              }
            , Cmd.map ReplySeenMsg cmd
            )

        SelectedSubpart (SubpartReply id) ->
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
                  if Form.isFinished preMsg then SelectedNothing else model.selected
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
      ( { model | selected = SelectedNothing }
      , Cmd.none
      )

    UnitInbox result -> onSuccess result <| \inbox ->
      let
        memberPairs =
          let
            fmm m =
              { index = MemberMessage m.id
              , title = case m.mtype of
                  Invitation -> "Invitation from " ++ m.right
                  Submission -> "Submission from " ++ m.left
              , description = m.text
              }

            frm r =
              { index = MemberReply r.id
              , title = case r.mtype of
                  Invitation -> "Reply from " ++ r.message.left
                  Submission -> "Reply from " ++ r.message.right
              , description = r.text
              }
          in
            List.map fmm inbox.messageMember ++
            List.map frm inbox.replyMember

        subpartPairs =
          let
            fms m =
              { index = SubpartMessage m.id
              , title = case m.mtype of
                  Invitation -> "Invitation from " ++ m.right
                  Submission -> "Submission from " ++ m.left
              , description = m.text
              }

            frs r =
              { index = SubpartReply r.id
              , title = case r.mtype of
                  Invitation -> "Reply from " ++ r.message.left
                  Submission -> "Reply from " ++ r.message.right
              , description = r.text
              }
          in
            List.map fms inbox.messageSubpart ++
            List.map frs inbox.replySubpart

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

    GetUnitInfo result -> onSuccess result <| \info ->
      ( { model | unit = Just info }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  case model.unit of
    Nothing ->
      [ Html.text "Loading..."
      ] ++ Notification.view model.notification

    Just unit ->
      if not unit.isAdmin
        then
          [ Html.text "You cannot edit this unit."
          ]
        else
          [ Html.h1
              [ Html.Attributes.class "title" ]
              [ Html.text <| unit.unit.name ++ " - Admin" ]
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
              Modal.view isActive CloseModal <|
                case model.selected of
                  SelectedNothing ->
                    Html.div [] []

                  SelectedMember memberId ->
                    Html.div [] <|
                      case memberId of
                        MemberMessage rid ->
                          case lookupById rid model.inbox.messageMember of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map ReplyMsg <|
                              Preview.make <|
                              ReplyForm.view msg model.previewMessage

                        MemberReply rid ->
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
                        SubpartMessage rid ->
                          case lookupById rid model.inbox.messageSubpart of
                            Nothing ->
                              [ Html.text "Error." ]
                            Just msg ->
                              List.singleton <|
                              Html.map ReplyMsg <|
                              Preview.make <|
                              ReplyForm.view msg model.previewMessage

                        SubpartReply rid ->
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
