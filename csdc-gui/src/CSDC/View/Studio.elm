module CSDC.View.Studio exposing
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
import CSDC.Component.Column as Column
import CSDC.Component.DotMenu as DotMenu
import CSDC.Component.Modal as Modal
import CSDC.Component.Panel as Panel
import CSDC.Component.Preview as Preview
import CSDC.Component.Progress as Progress
import CSDC.Form.Unit as UnitForm
import CSDC.Input exposing (button)
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.View.PreviewMessage as PreviewMessage
import CSDC.View.PreviewReply as PreviewReply
import CSDC.View.PreviewUnit as PreviewUnit

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
  | UnitCreateMsg UnitForm.Msg
  | UnitCreateOpen
  | UnitCreateClose
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
        Nothing ->
          ( model
          , Cmd.none
          )
        Just person ->
          case unitMsg of
            UnitForm.APIMsg (API.CreateUnit result) ->
              let
                initialUnitCreate = UnitForm.initial

                onSuccess = Notification.withResponse UnitForm.ResetNotification model.unitCreate

                (unitCreate, cmd) = onSuccess result <| \id ->
                  ( initialUnitCreate
                  , Page.goTo pageInfo (Page.ViewUnit id)
                  )
              in
                ( { model | unitCreate = unitCreate, unitCreateOpen = False }
                , Cmd.map UnitCreateMsg cmd
                )

            _ ->
              let
                (unitCreate, cmd) = UnitForm.update API.createUnit person.id unitMsg model.unitCreate
              in
                ( { model | unitCreate = unitCreate }
                , Cmd.map UnitCreateMsg cmd
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

        _ ->
          (model, Cmd.none)

--------------------------------------------------------------------------------
-- View

dotMenu : List (DotMenu.Item Msg)
dotMenu =
  [ { label = "Create New Unit"
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
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map UnitsMsg <| Panel.view model.panelUnits ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map MessagesMsg <| Panel.view model.panelMessages ]
          ]

      , Modal.view model.unitCreateOpen UnitCreateClose <| List.singleton <|
          Html.map UnitCreateMsg <|
          Preview.make
            [ Html.h2
                []
                [ Html.text "Create Unit" ]
            , UnitForm.view model.unitCreate
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

              SelectedInbox inboxId ->
                case inboxId of
                  ReplyMemberId id ->
                    case idMapLookup id model.inbox.replyMember of
                      Nothing ->
                        Html.text "Error."
                      Just msg ->
                        Html.map PreviewReplyMemberMsg (PreviewReply.view id msg)

                  _ ->
                    Html.text "OOO"

{-
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
