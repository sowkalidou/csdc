module Page.Unit exposing
  ( Model
  , initial
  , setup
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxImageText as BoxImageText
import UI.Column as Column
import UI.DotMenu as DotMenu
import UI.Modal as Modal
import UI.PreviewImageText as PreviewImageText
import UI.Progress as Progress
import UI.Tabs as Tabs
import Form.Unit as UnitForm
import Form.UnitDelete as UnitDeleteForm
import Form.Message as MessageForm
import Form.SubmissionMember as SubmissionMemberForm
import Notification exposing (Notification)
import Page as Page exposing (UnitTab)
import Types exposing (..)
import Page.UnitInfo as UnitInfo
import Page.UnitAdmin as UnitAdmin
import Page.UnitFiles as UnitFiles
import Page.UnitForum as UnitForum
import Form

import Html exposing (Html)
import Html.Attributes
import Markdown

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { info : Maybe UnitInfo
  , tab : Page.UnitTab
  , unitInfo : UnitInfo.Model
  , unitAdmin : UnitAdmin.Model
  , unitFiles : UnitFiles.Model
  , unitForum : UnitForum.Model
  , notification : Notification
  }

initial : Model
initial =
  { info = Nothing
  , tab = Page.UnitInfo
  , unitInfo = UnitInfo.initial
  , unitAdmin = UnitAdmin.initial
  , unitFiles = UnitFiles.initial
  , unitForum = UnitForum.initial
  , notification = Notification.Empty
  }

setup : Id Unit -> Page.UnitTab -> Cmd Msg
setup id tab =
  Cmd.batch
    [ Cmd.map (GetUnitInfo tab) <| API.getUnitInfo id
    ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = GetUnitInfo Page.UnitTab (API.Response UnitInfo)
  | SetTab Page.UnitTab
  | ResetNotification
  | UnitInfoMsg UnitInfo.Msg
  | UnitAdminMsg UnitAdmin.Msg
  | UnitFilesMsg UnitFiles.Msg
  | UnitForumMsg UnitForum.Msg

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse ResetNotification model
  in
  case msg of
    UnitInfoMsg umsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          let
            (unitInfo, cmd) = UnitInfo.update info pageInfo umsg model.unitInfo
          in
            ( { model | unitInfo = unitInfo }
            , Cmd.map UnitInfoMsg cmd
            )

    UnitAdminMsg umsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          let
            (unitAdmin, cmd) = UnitAdmin.update info pageInfo umsg model.unitAdmin
          in
            ( { model | unitAdmin = unitAdmin }
            , Cmd.map UnitAdminMsg cmd
            )

    UnitFilesMsg umsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          let
            (unitFiles, cmd) = UnitFiles.update info pageInfo umsg model.unitFiles
          in
            ( { model | unitFiles = unitFiles }
            , Cmd.map UnitFilesMsg cmd
            )

    UnitForumMsg umsg ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          let
            (unitForum, cmd) = UnitForum.update info pageInfo umsg model.unitForum
          in
            ( { model | unitForum = unitForum }
            , Cmd.map UnitForumMsg cmd
            )

    GetUnitInfo tab result -> onSuccess result <| \info ->
      ( let
          modelChoice = case model.info of
            Nothing -> initial
            Just old -> if old.id == info.id then model else initial
        in
          { modelChoice
          | info = Just info
          , tab = tab
          }
      , case tab of
          Page.UnitInfo -> Cmd.none
          Page.UnitAdmin -> Cmd.map UnitAdminMsg <| UnitAdmin.setup info.id
          Page.UnitFiles -> Cmd.map UnitFilesMsg <| UnitFiles.setup info.id
          Page.UnitForum mtid -> Cmd.map UnitForumMsg <| UnitForum.setup info.id mtid
      )

    SetTab tab ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          ( { model | tab = tab }
            , case tab of
                Page.UnitInfo -> setup info.id Page.UnitInfo
                Page.UnitAdmin -> Cmd.map UnitAdminMsg <| UnitAdmin.setup info.id
                Page.UnitFiles -> Cmd.map UnitFilesMsg <| UnitFiles.setup info.id
                Page.UnitForum mtid -> Cmd.map UnitForumMsg <| UnitForum.setup info.id mtid
          )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
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
      [ Html.div
          [ Html.Attributes.class "is-flex is-justify-content-space-between" ]
          [ Html.h1
              [ Html.Attributes.class "title" ]
              [ Html.text info.unit.name ]
          , Html.map SetTab <|
            Tabs.view (sameTab model.tab) <| List.concat
              [ if info.isMember
                then
                  [ (Page.UnitInfo, "Information")
                  , (Page.UnitForum Nothing, "Forum")
                  , (Page.UnitFiles, "Files")
                  ]
                else []
              , if info.isAdmin
                then [(Page.UnitAdmin, "Admin")]
                else []
              ]
          ]
      , Html.div
          [ Html.Attributes.style "height" "100%"
          ] <| case model.tab of
          Page.UnitInfo ->
            List.map (Html.map UnitInfoMsg) <|
            UnitInfo.view info model.unitInfo
          Page.UnitAdmin ->
            List.map (Html.map UnitAdminMsg) <|
            UnitAdmin.view info model.unitAdmin
          Page.UnitForum _ ->
            List.map (Html.map UnitForumMsg) <|
            UnitForum.view info model.unitForum
          Page.UnitFiles ->
            List.map (Html.map UnitFilesMsg) <|
            UnitFiles.view info model.unitFiles

      ] ++ Notification.view model.notification

sameTab : Page.UnitTab -> Page.UnitTab -> Bool
sameTab t1 t2 =
  case (t1, t2) of
    (Page.UnitInfo, Page.UnitInfo) -> True
    (Page.UnitAdmin, Page.UnitAdmin) -> True
    (Page.UnitFiles, Page.UnitFiles) -> True
    (Page.UnitForum _, Page.UnitForum _) -> True
    _ -> False
