module CSDC.View.UnitForum exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.API as API
import CSDC.UI.BoxPost as BoxPost
import CSDC.UI.BoxThread as BoxThread
import CSDC.UI.Modal as Modal
import CSDC.UI.Column as Column
import CSDC.UI.Preview as Preview
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

type alias Model =
  { threads : List ThreadInfo
  , posts : List PostInfo
  , selected : Maybe (Id Thread)
  , notification : Notification
  }

initial : Model
initial =
  { threads = []
  , posts = []
  , selected = Nothing
  , notification = Notification.Empty
  }

setup : Id Unit -> Cmd Msg
setup id = Cmd.batch
  [ Cmd.map SetThreads <| API.getUnitThreads id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetThreads (API.Response (List ThreadInfo))
  | SetPosts (API.Response (List PostInfo))
  | SelectThread (Id Thread)
  | NewThread
  | NewPost
  | ResetNotification

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
  let
    onSuccess = Notification.withResponse ResetNotification model
  in
  case msg of
    SetThreads result -> onSuccess result <| \threads ->
      ( { model | threads = threads }
      , Cmd.none
      )

    SetPosts result -> onSuccess result <| \posts ->
      ( { model | posts = posts }
      , Cmd.none
      )

    SelectThread selected ->
      ( { model | selected = Just selected, posts = [] }
      , Cmd.map SetPosts <| API.getThreadPosts selected
      )

    NewThread ->
      ( model
      , Cmd.none
      )

    NewPost ->
      ( model
      , Cmd.none
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : UnitInfo -> Model -> List (Html Msg)
view unit model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Column.view "Threads" [] (viewThreads model.selected model.threads) ]
      , Html.div
          [ Html.Attributes.class "column is-half" ]
          [ Column.view "Posts" [] (viewPosts model.posts) ]
      ]
  ] ++
  Notification.view model.notification

viewThreads : Maybe (Id Thread) -> List ThreadInfo -> List (Html Msg)
viewThreads tid threads =
  let
    toBox thread =
      Html.map SelectThread <|
      BoxThread.view (tid == Just thread.id) thread
  in
    List.map toBox threads

viewPosts : List PostInfo -> List (Html Msg)
viewPosts = List.map BoxPost.view

