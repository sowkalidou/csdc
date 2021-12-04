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
import CSDC.Page as Page
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

type Tab = Info | Admin | Forum

type alias Model =
  { info : Maybe UnitInfo
  , tab : Tab
  , unitInfo : UnitInfo.Model
  , unitAdmin : UnitAdmin.Model
  , unitForum : UnitForum.Model
  , notification : Notification
  }

initial : Model
initial =
  { info = Nothing
  , tab = Info
  , unitInfo = UnitInfo.initial
  , unitAdmin = UnitAdmin.initial
  , unitForum = UnitForum.initial
  , notification = Notification.Empty
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
  | SetTab Tab
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

    GetUnitInfo result -> onSuccess result <| \info ->
      ( { model | info = Just info, tab = Info }
      , Cmd.none
      )

    SetTab tab ->
      case model.info of
        Nothing -> (model, Cmd.none)
        Just info ->
          ( { model | tab = tab }
            , case tab of
                Info -> setup info.id
                Admin -> Cmd.map UnitAdminMsg <| UnitAdmin.setup info.id
                Forum -> Cmd.map UnitForumMsg <| UnitForum.setup info.id
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
            Tabs.view model.tab <| List.concat
              [ [(Info, "Information")]
              , if info.isMember
                then [(Forum, "Forum")]
                else []
              , if info.isAdmin
                then [(Admin, "Admin")]
                else []
              ]
          ]
      , Html.div
          [ Html.Attributes.style "height" "100%"
          ] <| case model.tab of
          Info ->
            List.map (Html.map UnitInfoMsg) <|
            UnitInfo.view info model.unitInfo
          Admin ->
            List.map (Html.map UnitAdminMsg) <|
            UnitAdmin.view info model.unitAdmin
          Forum ->
            List.map (Html.map UnitForumMsg) <|
            UnitForum.view info model.unitForum

      ] ++ Notification.view model.notification

