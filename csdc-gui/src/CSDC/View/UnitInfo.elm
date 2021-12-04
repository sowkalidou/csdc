module CSDC.View.UnitInfo exposing
  ( Model
  , initial
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
import CSDC.UI.Tabs as Tabs
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

type Tab = Info | Admin | Forum

type Selected
  = SelectedPerson (Id Person)
  | SelectedUnit (Id Unit)

type alias Model =
  { notification : Notification
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
  { notification = Notification.Empty
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

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetSelected Selected
  | ViewSelected Selected
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

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
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

    CloseModal ->
      ( { model | selected = Nothing }
      , Cmd.none
      )

    UnitEditOpen ->
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
      let
        config =
          { request = API.updateUnit info.id
          , finish = \_ -> Page.goTo pageInfo (Page.Unit info.id)
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
      let
        config =
          { request = API.deleteUnit info.id
          , finish = Page.goTo pageInfo (Page.Unit info.id)
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
      let
        config =
          { finish = Page.goTo pageInfo <| Page.Unit info.id
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
        , subpartCreate = MessageForm.fromUnitInfo info
        }
      , Cmd.none
      )

    SubpartCreateClose ->
      ( { model | subpartCreateOpen = False }
      , Cmd.none
      )

    SubpartCreateMsg subpartMsg ->
      let
        config =
          { request = API.sendMessageSubpart
          , finish = Page.goTo pageInfo <| Page.Unit info.id
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

--------------------------------------------------------------------------------
-- View

view : UnitInfo -> Model -> List (Html Msg)
view info model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view
              "Information"
              [ DotMenu.make <| List.concat
                  [ if info.isMember || info.isMembershipPending
                    then []
                    else
                      [ { label = "Become a member"
                        , message = SubmissionMemberOpen
                        }
                      ]


                  , if List.isEmpty info.unitsForMessage
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
                      , { label = "Delete this unit"
                        , message = UnitDeleteOpen
                        }
                      ]
                    else []
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
