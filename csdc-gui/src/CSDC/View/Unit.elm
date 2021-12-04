module CSDC.View.Unit exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.UI.BoxImageText as BoxImageText
import CSDC.UI.Column as Column
import CSDC.UI.DotMenu as DotMenu
import CSDC.UI.Modal as Modal
import CSDC.UI.PreviewImageText as PreviewImageText
import CSDC.UI.Progress as Progress
import CSDC.Form.Unit as UnitForm
import CSDC.Form.UnitDelete as UnitDeleteForm
import CSDC.Form.Message as MessageForm
import CSDC.Form.SubmissionMember as SubmissionMemberForm
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Page as Page
import CSDC.Types exposing (..)
import Form

import Html exposing (Html)
import Html.Attributes
import Markdown

--------------------------------------------------------------------------------
-- Model

type Selected
  = SelectedPerson (Id Person)
  | SelectedUnit (Id Unit)

type alias Model =
  { info : Maybe UnitInfo
  , notification : Notification
  , selected : Maybe Selected
  , invited : Maybe (Id Unit)
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
  , notification = Notification.Empty
  , selected = Nothing
  , invited = Nothing
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
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUnitInfo (API.Response UnitInfo)
  | SetSelected Selected
  | ViewSelected Selected
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
    SetSelected selected ->
      ( { model | selected = Just selected }
      , Cmd.none
      )

    ViewSelected selected ->
      case selected of
        SelectedPerson id ->
          ( initial
          , Page.goTo pageInfo (Page.Person id)
          )

        SelectedUnit id ->
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
      ( { model | selected = Nothing }
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
              { finish = Page.goTo pageInfo <| Page.Unit unit.id
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
        , subpartCreate = case model.info of
            Nothing -> MessageForm.initial
            Just info -> MessageForm.fromUnitInfo info
        }
      , Cmd.none
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
              , finish = Page.goTo pageInfo <| Page.Unit unit.id
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
      ( { model | info = Just info }
      , Cmd.none
      )

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
          [ Html.text info.unit.name ]
      , Html.div
          [ Html.Attributes.class "columns"
          , Html.Attributes.style "height" "100%"
          ]
          [ Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view
                  "Information"
                  [ DotMenu.make <| List.concat
                      [ if List.isEmpty info.unitsForMessage
                        then
                          []
                        else
                          [ { label = "Invitation for this unit"
                            , message = SubpartCreateOpen Invitation
                            }
                          , { label = "Submission to this unit"
                            , message = SubpartCreateOpen Submission
                            }
                          ]

                      , if info.isAdmin
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

                      , if info.isMember || info.isMembershipPending
                          then []
                          else
                            [ { label = "Become a member"
                              , message = SubmissionMemberOpen
                              }
                            ]
                      ]
                  ]
                  [ Html.div
                      []
                      [ Html.strong [] [ Html.text "Chair: " ]
                      , Html.text <|
                          case lookup (\unitMember -> unitMember.id == info.unit.chair) info.members of
                            Nothing -> "Loading..."
                            Just unitMember -> unitMember.person.name
                      ]
                  , Html.div [] <|
                      if info.isMembershipPending
                      then [ Html.text "Your submission was sent." ]
                      else []
                  , Markdown.toHtml [] info.unit.description
                  ]
              ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view "Sub-Units" [] (viewUnits info.children) ]
          , Html.div
              [ Html.Attributes.class "column is-one-third" ]
              [ Column.view "Members" [] (viewPersons info.members) ]
          ]

      , Modal.view model.unitEditOpen UnitEditClose <|
          Html.map UnitEditMsg <|
          Form.viewWith "Edit Profile" UnitForm.view model.unitEdit

      , Modal.view model.unitDeleteOpen UnitDeleteClose <|
          Html.map UnitDeleteMsg <|
          Form.viewWith "Delete Unit" UnitDeleteForm.view model.unitDelete

      , Modal.view model.submissionMemberOpen SubmissionMemberClose <|
          Html.map SubmissionMemberMsg <|
          let
            member = { person = info.user, unit = info.id }
          in
            Form.viewWith "Send Submission"(SubmissionMemberForm.view member) model.submissionMember

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
            Nothing -> False
            _ -> True
        in
          Modal.view isActive CloseModal <|
            case model.selected of
              Nothing ->
                Html.div [] []

              Just (SelectedPerson id) ->
                case lookupById id info.members of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just unitMember ->
                    PreviewImageText.view unitMember.person <|
                    ViewSelected (SelectedPerson unitMember.id)

              Just (SelectedUnit id) ->
                case lookupById id info.children of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just unitSubpart ->
                    PreviewImageText.view unitSubpart.unit <|
                    ViewSelected (SelectedUnit unitSubpart.id)

      ] ++ Notification.view model.notification

viewUnits : List UnitSubpart -> List (Html Msg)
viewUnits subparts =
  let
    toBox subpart =
      Html.map (SetSelected << SelectedUnit) <|
      BoxImageText.view False subpart.id subpart.unit
  in
    List.map toBox subparts

viewPersons : List UnitMember -> List (Html Msg)
viewPersons members =
  let
    toBox member =
      Html.map (SetSelected << SelectedPerson) <|
      BoxImageText.view False member.id member.person
  in
    List.map toBox members
