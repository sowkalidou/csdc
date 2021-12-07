module Page.UnitForum exposing
  ( Model
  , setup
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import UI.BoxPost as BoxPost
import UI.BoxThread as BoxThread
import UI.Modal as Modal
import UI.Column as Column
import Notification exposing (Notification)
import Page as Page exposing (UnitTab (..))
import Types exposing (..)
import Form.Thread as ThreadForm
import Form.Post as PostForm
import Form

import Browser.Dom as Dom
import Delay
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { threads : List ThreadInfo
  , posts : List PostInfo
  , selected : Maybe (Id Thread)
  , threadForm : ThreadForm.Model
  , threadFormOpen : Bool
  , postForm : PostForm.Model
  , postFormOpen : Bool
  , notification : Notification
  }

initial : Model
initial =
  { threads = []
  , posts = []
  , selected = Nothing
  , threadForm = ThreadForm.initial
  , threadFormOpen = False
  , postForm = PostForm.initial
  , postFormOpen = False
  , notification = Notification.Empty
  }

setup : Id Unit -> Maybe (Id Thread) -> Cmd Msg
setup id mtid = Cmd.batch
  [ Cmd.map (SetThreads mtid) <| API.getUnitThreads id
  ]

--------------------------------------------------------------------------------
-- Update

type Msg
  = SetThreads (Maybe (Id Thread)) (API.Response (List ThreadInfo))
  | SetPosts (API.Response (List PostInfo))
  | SelectThread (Id Thread)
  | ThreadFormMsg ThreadForm.Msg
  | ThreadFormOpen
  | ThreadFormClose
  | PostFormMsg PostForm.Msg
  | PostFormOpen
  | PostFormClose
  | Focus
  | FocusResult (Result Dom.Error ())
  | ResetNotification

update : UnitInfo -> Page.Info -> Msg -> Model -> (Model, Cmd Msg)
update info pageInfo msg model =
  let
    onSuccess = Notification.withResponse ResetNotification model
    reload tid = Page.goTo pageInfo <| Page.Unit (UnitForum tid) info.id
  in
  case msg of
    SetThreads mtid result -> onSuccess result <| \threads ->
      ( { model
        | threads = threads
        , selected = case mtid of
            Just _ -> mtid
            Nothing -> model.selected
        }
      , case mtid of
          Nothing ->
            case model.selected of
              Nothing ->
                Cmd.none
              Just tid ->
                Cmd.map SetPosts <| API.getThreadPosts tid
          Just selected ->
            Cmd.map SetPosts <| API.getThreadPosts selected
      )

    SetPosts result -> onSuccess result <| \posts ->
      ( { model | posts = posts }
      , Delay.after 0.1 Delay.Second Focus
      )

    SelectThread selected ->
      ( { model | selected = Just selected, posts = [] }
      , Cmd.map SetPosts <| API.getThreadPosts selected
      )

    ThreadFormOpen ->
      ( { model | threadFormOpen = True }
      , Cmd.none
      )

    ThreadFormClose ->
      ( { model | threadFormOpen = False }
      , Cmd.none
      )

    ThreadFormMsg unitMsg ->
      let
        config =
          { id = info.id
          , finish = \tid -> reload (Just tid)
          }
        (threadForm, cmd) = ThreadForm.updateWith config unitMsg model.threadForm
      in
        ( { model
          | threadForm = threadForm
          , threadFormOpen = not (Form.isFinished unitMsg)
          }
        , Cmd.map ThreadFormMsg cmd
        )

    PostFormOpen ->
      ( { model | postFormOpen = True }
      , Cmd.none
      )

    PostFormClose ->
      ( { model | postFormOpen = False }
      , Cmd.none
      )

    PostFormMsg unitMsg ->
      case model.selected of
        Nothing ->
          (model, Cmd.none)
        Just id ->
          let
            config =
              { id = id
              , finish = Cmd.batch
                  [ reload Nothing
                  ]
              }
            (postForm, cmd) = PostForm.updateWith config unitMsg model.postForm
          in
            ( { model
              | postForm = postForm
              , postFormOpen = not (Form.isFinished unitMsg)
              }
            , Cmd.map PostFormMsg cmd
            )

    Focus ->
      ( model
      , Dom.getElement "Posts-items"
        |> Task.andThen (\p -> Dom.setViewportOf "Posts-items" 0 p.scene.height)
        |> Task.attempt FocusResult
      )

    FocusResult r ->
      ( model
      , case r of
          Err e -> Cmd.none --Debug.log ("Could not focus: " ++ Debug.toString e) Cmd.none
          _ -> Cmd.none
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
          [ Html.Attributes.class "column is-one-third" ]
          [ Column.view "Threads" [smallButton "New Thread" ThreadFormOpen] <|
            viewThreads model.selected model.threads
          ]
      , Html.div
          [ Html.Attributes.class "column is-two-thirds" ]
          [ if List.isEmpty model.posts
            then
              Html.div [] []
            else
              Column.view "Posts" [smallButton "New Post" PostFormOpen] <|
              viewPosts model.posts
          ]
      ]

      , Modal.view model.threadFormOpen ThreadFormClose <|
          Html.map ThreadFormMsg <|
          Form.viewWith "New Thread" ThreadForm.view model.threadForm

      , Modal.view model.postFormOpen PostFormClose <|
          Html.map PostFormMsg <|
          Form.viewWith "New Post" PostForm.view model.postForm

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

smallButton : String -> msg -> Html msg
smallButton txt msg =
  Html.button
    [ Html.Attributes.class "button is-success is-small"
    , Html.Events.onClick msg
    ]
    [ Html.text txt
    ]

