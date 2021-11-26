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
  | SelectedPerson (Id Member)
  | SelectedUnit (Id Subpart)

type ViewSelected
  = ViewSelectedPerson (Id Person)
  | ViewSelectedUnit (Id Unit)

type alias Model =
  { info : Maybe UnitInfo
  , panelChildren : Panel.Model (Id Subpart)
  , panelMembers : Panel.Model (Id Member)
  , notification : Notification
  , selected : Selected
  , invited : Maybe (Id Unit)
  , inbox : Inbox
  , unitEdit : UnitForm.Model
  , unitEditOpen : Bool
  , unitDeleteOpen : Bool
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
  , unitDeleteOpen = False
  }

setup : Id Unit -> Cmd Msg
setup id =
  Cmd.batch
    [ Cmd.map APIMsg <| API.getUnitInfo id
    , Cmd.map APIMsg <| API.unitInbox id
    ]

canEdit : Maybe PersonInfo -> Model -> Bool
canEdit mid model =
  case mid of
    Nothing -> False
    Just pinfo ->
      case model.info of
        Nothing -> False
        Just info ->
          case idMapFind (\w -> w.id == info.unit.chair) info.members of
            Nothing -> False
            Just member -> pinfo.id == member.id

isMember : Maybe PersonInfo -> Model -> Maybe (WithId Person)
isMember mid model =
  case mid of
    Just pinfo ->
      case model.info of
        Nothing -> Nothing
        Just info ->
          if idMapAny (\user -> user.id == pinfo.id) info.members
          then Nothing
          else Just { id = pinfo.id, value = pinfo.person }
    _ ->
      Nothing

isMemberPending : Maybe PersonInfo -> Model -> Bool
isMemberPending mid model =
  case mid of
    Just info ->
      idMapAny (\m -> m.value.person == info.id) model.inbox.messageMember
    _ ->
      False

--------------------------------------------------------------------------------
-- Update

type Msg
  = APIMsg API.Msg
  | SubpartsMsg (Panel.Msg (Id Subpart))
  | MembersMsg (Panel.Msg (Id Member))
  | View ViewSelected
  | MessageMember (Id Person) (Id Unit) MessageType
  | MessageSubpart (Id Person) (Id Unit) MessageType
  | ViewAdmin (Id Unit)
  | CloseModal
  | UnitEditMsg (UnitForm.Msg ())
  | UnitEditOpen
  | UnitEditClose
  | UnitDeleteConfirm
  | UnitDeleteOpen
  | UnitDeleteClose
  | Reset

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
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

    MessageMember pid uid mtype ->
      ( model
      , Page.goTo pageInfo (Page.MessageMember pid uid mtype)
      )

    MessageSubpart pid uid mtype ->
      ( model
      , Page.goTo pageInfo (Page.MessageSubpart pid uid mtype)
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

    UnitDeleteConfirm ->
      case model.info of
        Nothing ->
          ( model
          , Cmd.none
          )
        Just unit ->
          ( model
          , Cmd.map APIMsg <| API.deleteUnit unit.id
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
        API.GetUnitInfo result -> onSuccess result <| \info ->
          let
            pairsMembers =
              idMapToList info.members |>
              List.map (\(id,withid) ->
                { index = id
                , title = withid.value.name
                , description = withid.value.description
                }
              )

            panelMembers =
              Panel.update (Panel.SetItems pairsMembers) model.panelMembers

            pairsChildren =
              idMapToList info.children |>
              List.map (\(id,withid) ->
                { index = id
                , title = withid.value.name
                , description = withid.value.description
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

        API.UnitInbox _ result -> onSuccess result <| \inbox ->
          ( { model | inbox = inbox }
          , Cmd.none
          )

        API.DeleteUnit result -> onSuccess result <| \_ ->
          ( { model | unitDeleteOpen = False }
          , Page.goTo pageInfo Page.Studio
          )

        API.SendMessageSubpart result -> onSuccess result <| \_ ->
          ( model
          , case model.info of
              Nothing -> Cmd.none
              Just info -> Cmd.map APIMsg <| API.unitInbox info.id
          )

        _ ->
          (model, Cmd.none)

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
                              , message = MessageSubpart pinfo.id info.id Invitation
                              }
                            , { label = "Submission to this unit"
                              , message = MessageSubpart pinfo.id info.id Submission
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
                              , message = MessageMember wid.id info.id Submission
                              }
                            ]
                      ]
                  )
                  [ Html.div
                      []
                      [ Html.strong [] [ Html.text "Chair: " ]
                      , Html.text <|
                          case idMapFind (\w -> w.id == info.unit.chair) info.members of
                            Nothing -> "Loading..."
                            Just withid -> withid.value.name
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
          Preview.make
            [ Html.h2
                []
                [ Html.text "Delete Unit" ]
            , Html.p
                []
                [ Html.text "Are you sure you want to delete this unit?" ]
            , Html.p
                []
                [ Html.text "This operation is not reversible." ]
            , Input.buttonDanger UnitDeleteConfirm "Delete"
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

              SelectedPerson id ->
                case idMapLookup id info.members of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just person ->
                    PersonPreview.view person.value <|
                    View (ViewSelectedPerson person.id)

              SelectedUnit id ->
                case idMapLookup id info.children of
                  Nothing ->
                    Html.div [] [ Html.text "Loading..." ]
                  Just subunit ->
                    UnitPreview.view subunit.value <|
                    View (ViewSelectedUnit subunit.id)

      ] ++ Notification.view model.notification
