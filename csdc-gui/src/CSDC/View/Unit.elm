module CSDC.View.Unit exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
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
import CSDC.Form.UnitDelete as UnitDeleteForm
import CSDC.Form.Message as MessageForm
import CSDC.Form.SubmissionMember as SubmissionMemberForm
import CSDC.Input as Input
import CSDC.Notification as Notification
import CSDC.Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import CSDC.View.PersonPreview as PersonPreview
import CSDC.View.UnitPreview as UnitPreview
import Form

import Html exposing (Html)
import Html.Attributes
import Html.Events
import String
import Tuple exposing (pair)

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedNothing
  | SelectedPerson (Id Person)
  | SelectedUnit (Id Unit)

type ViewSelected
  = ViewSelectedPerson (Id Person)
  | ViewSelectedUnit (Id Unit)

type alias Model =
  { info : Maybe UnitInfo
  , panelChildren : Panel.Model (Id Unit)
  , panelMembers : Panel.Model (Id Person)
  , notification : Notification
  , selected : Selected
  , invited : Maybe (Id Unit)
  , inbox : Inbox
  , unitEdit : UnitForm.Model
  , unitEditOpen : Bool
  , unitDelete : UnitDeleteForm.Model
  , unitDeleteOpen : Bool
  , submissionMember : SubmissionMemberForm.Model
  , submissionMemberOpen : Bool
  , subpartCreate : MessageForm.Model
  , subpartCreateOpen : Bool
  , subpartCreateType : MessageType
  }

initial : Model
initial =
  { info = Nothing
  , panelChildren = Panel.initial "Sub-Units"
  , panelMembers = Panel.initial "Members"
  , notification = Notification.Empty
  , selected = SelectedNothing
  , invited = Nothing
  , inbox = emptyInbox
  , unitEdit = UnitForm.initial
  , unitEditOpen = False
  , unitDelete = UnitDeleteForm.initial
  , unitDeleteOpen = False
  , submissionMember = SubmissionMemberForm.initial
  , submissionMemberOpen = False
  , subpartCreate = MessageForm.initial
  , subpartCreateOpen = False
  , subpartCreateType = Invitation
  }

setup : Id Unit -> Cmd Msg
setup id =
  Cmd.batch
    [ Cmd.map GetUnitInfo <| API.getUnitInfo id
    , Cmd.map UnitInbox <| API.unitInbox id
    ]

canEdit : Maybe PersonInfo -> Model -> Bool
canEdit mid model =
  case mid of
    Nothing -> False
    Just pinfo ->
      case model.info of
        Nothing -> False
        Just info ->
          case lookup (\w -> w.id == info.unit.chair) info.members of
            Nothing -> False
            Just member -> pinfo.id == member.id

isMember : Maybe PersonInfo -> Model -> Maybe (WithId Person)
isMember mid model =
  case mid of
    Just pinfo ->
      case model.info of
        Nothing -> Nothing
        Just info ->
          if List.any (\unitMember -> unitMember.id == pinfo.id) info.members
          then Nothing
          else Just { id = pinfo.id, value = pinfo.person }
    _ ->
      Nothing

isMemberPending : Maybe PersonInfo -> Model -> Bool
isMemberPending mid model =
  case mid of
    Just info ->
      List.any (\m -> m.value.person == info.id) model.inbox.messageMember
    _ ->
      False

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUnitInfo (API.Response UnitInfo)
  | UnitInbox (API.Response Inbox)
  | SubpartsMsg (Panel.Msg (Id Unit))
  | MembersMsg (Panel.Msg (Id Person))
  | View ViewSelected
  | ViewAdmin (Id Unit)
  | CloseModal
  | UnitEditMsg (UnitForm.Msg ())
  | UnitEditOpen
  | UnitEditClose
  | UnitDeleteMsg UnitDeleteForm.Msg
  | UnitDeleteOpen
  | UnitDeleteClose
  | SubmissionMemberMsg SubmissionMemberForm.Msg
  | SubmissionMemberOpen
  | SubmissionMemberClose
  | SubpartCreateMsg (MessageForm.Msg NewSubpart)
  | SubpartCreateOpen MessageType
  | SubpartCreateClose
  | Reset

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse Reset model
  in
  case msg of
    SubpartsMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelChildren = Panel.update m model.panelChildren
            , selected = SelectedUnit id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelChildren = Panel.update m model.panelChildren }
          , Cmd.none
          )

    MembersMsg m ->
      case m of
        Panel.SetSelected (Just id) ->
          ( { model
            | panelMembers = Panel.update m model.panelMembers
            , selected = SelectedPerson id
            }
          , Cmd.none
          )

        _ ->
          ( { model | panelMembers = Panel.update m model.panelMembers }
          , Cmd.none
          )

    View selected ->
      case selected of
        ViewSelectedPerson id ->
          ( initial
          , Page.goTo pageInfo (Page.Person id)
          )

        ViewSelectedUnit id ->
          ( initial
          , Page.goTo pageInfo (Page.Unit id)
          )

    ViewAdmin _ ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just unit ->
          ( model
          , Page.goTo pageInfo (Page.UnitAdmin unit.id)
          )

    CloseModal ->
      ( { model | selected = SelectedNothing }
      , Cmd.none
      )

    UnitEditOpen ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just info ->
          ( { model
            | unitEditOpen = True
            , unitEdit = UnitForm.fromUnit info.unit
            }
          , Cmd.none
          )

    UnitEditClose ->
      ( { model | unitEditOpen = False }
      , Cmd.none
      )

    UnitEditMsg unitMsg ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just unit ->
          let
            config =
              { request = API.updateUnit unit.id
              , finish = \_ -> Page.goTo pageInfo (Page.Unit unit.id)
              }
            (unitEdit, cmd) = UnitForm.updateWith config unitMsg model.unitEdit
          in
            ( { model
              | unitEdit = unitEdit
              , unitEditOpen = not (Form.isFinished unitMsg)
              }
            , Cmd.map UnitEditMsg cmd
            )

    UnitDeleteOpen ->
      ( { model | unitDeleteOpen = True }
      , Cmd.none
      )

    UnitDeleteClose ->
      ( { model | unitDeleteOpen = False }
      , Cmd.none
      )

    UnitDeleteMsg unitMsg ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just unit ->
          let
            config =
              { request = API.deleteUnit unit.id
              , finish = Page.goTo pageInfo (Page.Unit unit.id)
              }
            (unitDelete, cmd) = UnitDeleteForm.updateWith config unitMsg model.unitDelete
          in
            ( { model
              | unitDelete = unitDelete
              , unitDeleteOpen = not (Form.isFinished unitMsg)
              }
            , Cmd.map UnitDeleteMsg cmd
            )

    SubmissionMemberOpen ->
      ( { model | submissionMemberOpen = True }
      , Cmd.none
      )

    SubmissionMemberClose ->
      ( { model | submissionMemberOpen = False }
      , Cmd.none
      )

    SubmissionMemberMsg subpartMsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just unit ->
          let
            config =
              { finish = Cmd.none
              }
            (submissionMember, cmd) = SubmissionMemberForm.updateWith config subpartMsg model.submissionMember
          in
            ( { model
              | submissionMember = submissionMember
              , submissionMemberOpen = not (Form.isFinished subpartMsg)
              }
            , Cmd.map SubmissionMemberMsg cmd
            )

    SubpartCreateOpen mtype ->
      ( { model
        | subpartCreateOpen = True
        , subpartCreateType = mtype
        }
      , Cmd.map SubpartCreateMsg MessageForm.setup
      )

    SubpartCreateClose ->
      ( { model | subpartCreateOpen = False }
      , Cmd.none
      )

    SubpartCreateMsg subpartMsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just unit ->
          let
            config =
              { request = API.sendMessageSubpart
              , finish = Cmd.none
              }
            (subpartCreate, cmd) = MessageForm.updateWith config subpartMsg model.subpartCreate
          in
            ( { model
              | subpartCreate = subpartCreate
              , subpartCreateOpen = not (Form.isFinished subpartMsg)
              }
            , Cmd.map SubpartCreateMsg cmd
            )

    Reset ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    GetUnitInfo result -> onSuccess result <| \info ->
      let
        pairsMembers =
          info.members |>
          List.map (\unitMember ->
            { index = unitMember.id
            , title = unitMember.person.name
            , description = unitMember.person.description
            }
          )

        panelMembers =
          Panel.update (Panel.SetItems pairsMembers) model.panelMembers

        pairsChildren =
          info.children |>
          List.map (\unitSubpart ->
            { index = unitSubpart.id
            , title = unitSubpart.unit.name
            , description = unitSubpart.unit.description
            }
          )

        panelChildren =
          Panel.update (Panel.SetItems pairsChildren) model.panelChildren
      in
        ( { model
          | info = Just info
          , panelMembers = panelMembers
          , panelChildren = panelChildren
          , selected = SelectedNothing
          }
        , Cmd.none
        )

    UnitInbox result -> onSuccess result <| \inbox ->
      ( { model | inbox = inbox }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Maybe PersonInfo -> Model -> List (Html Msg)
view mid model =
  case model.info of
    Nothing ->
      [ Progress.view
      ] ++ Notification.view model.notification

    Just info ->
      [ Html.h1
          [ Html.Attributes.class "title" ]
          [ Html.text info.unit.name ]
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.make
                  "Information"
                  ( List.concat
                      [ case mid of
                          Just pinfo ->
                            [ { label = "Invitation for this unit"
                              , message = SubpartCreateOpen Invitation
                              }
                            , { label = "Submission to this unit"
                              , message = SubpartCreateOpen Submission
                              }
                            ]
                          _ ->
                            []
                      , if canEdit mid model
                          then
                            [ { label = "Edit profile"
                              , message = UnitEditOpen
                              }
                            , { label = "Admin"
                              , message = ViewAdmin info.id
                              }
                            , { label = "Delete this unit"
                              , message = UnitDeleteOpen
                              }
                            ]
                          else []
                      , case isMember mid model of
                          Nothing -> []
                          Just wid ->
                            [ { label = "Become a member"
                              , message = SubmissionMemberOpen
                              }
                            ]
                      ]
                  )
                  [ Html.div
                      []
                      [ Html.strong [] [ Html.text "Chair: " ]
                      , Html.text <|
                          case lookup (\unitMember -> unitMember.id == info.unit.chair) info.members of
                            Nothing -> "Loading..."
                            Just unitMember -> unitMember.person.name
                      ]
                  , Html.div
                      [ Html.Attributes.style "white-space" "pre-wrap"
                      ]
                      [ Html.strong [] [ Html.text "Description: " ]
                      , Html.text info.unit.description
                      ]
                  , Html.div [] <|
                      if isMemberPending mid model
                      then [ Html.text "Your submission was sent." ]
                      else []
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map SubpartsMsg <| Panel.view model.panelChildren ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Html.map MembersMsg <| Panel.view model.panelMembers ]
          ]

      , Modal.view model.unitEditOpen UnitEditClose <|
          Html.map UnitEditMsg <|
          Form.viewWith "Edit Profile" UnitForm.view model.unitEdit

      , Modal.view model.unitDeleteOpen UnitDeleteClose <|
          Html.map UnitDeleteMsg <|
          Form.viewWith "Delete Unit" UnitDeleteForm.view model.unitDelete

      , Modal.view model.submissionMemberOpen SubmissionMemberClose <|
          Html.map SubmissionMemberMsg <|
          case mid of
            Just person ->
              let
                member = { person = person.id, unit = info.id }
              in
                Form.viewWith "Send Submission"(SubmissionMemberForm.view member) model.submissionMember
            Nothing ->
              Html.text "Error"

      , Modal.view model.subpartCreateOpen SubpartCreateClose <|
          Html.map SubpartCreateMsg <|
          let
            make =
              case model.subpartCreateType of
                Invitation -> \uid -> { child = info.id, parent = uid }
                Submission -> \uid -> { child = uid, parent = info.id }

            title =
              case model.subpartCreateType of
                Invitation -> "Send Invitation"
                Submission -> "Send Submission"
          in
            Form.viewWith title (MessageForm.view model.subpartCreateType make) model.subpartCreate

      , let
          isActive = case model.selected of
            SelectedNothing -> False
            _ -> True
        in
          Modal.view isActive CloseModal <|
            case model.selected of
              SelectedNothing ->
                Html.div [] []

              SelectedPerson id ->
                case lookupById id info.members of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just unitMember ->
                    PersonPreview.view unitMember.person <|
                    View (ViewSelectedPerson unitMember.id)

              SelectedUnit id ->
                case lookupById id info.children of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just unitSubpart ->
                    UnitPreview.view unitSubpart.unit <|
                    View (ViewSelectedUnit unitSubpart.id)

      ] ++ Notification.view model.notification
