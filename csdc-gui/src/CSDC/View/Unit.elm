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
import CSDC.UI.Tabs as Tabs
import CSDC.Form.Unit as UnitForm
import CSDC.Form.UnitDelete as UnitDeleteForm
import CSDC.Form.Message as MessageForm
import CSDC.Form.SubmissionMember as SubmissionMemberForm
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Page as Page exposing (UnitTab)
import CSDC.Types exposing (..)
import CSDC.View.UnitInfo as UnitInfo
import CSDC.View.UnitAdmin as UnitAdmin
import CSDC.View.UnitForum as UnitForum
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
  , unitForum : UnitForum.Model
  , notification : Notification
  }

initial : Model
initial =
  { info = Nothing
  , tab = Page.UnitInfo
  , unitInfo = UnitInfo.initial
  , unitAdmin = UnitAdmin.initial
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
              [ [(Page.UnitInfo, "Information")]
              , if info.isMember
                then [(Page.UnitForum Nothing, "Forum")]
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

      ] ++ Notification.view model.notification

sameTab : Page.UnitTab -> Page.UnitTab -> Bool
sameTab t1 t2 =
  case (t1, t2) of
    (Page.UnitInfo, Page.UnitInfo) -> True
    (Page.UnitAdmin, Page.UnitAdmin) -> True
    (Page.UnitForum _, Page.UnitForum _) -> True
    _ -> False
