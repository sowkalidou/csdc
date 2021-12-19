module Page.SignIn exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import API as API
import Form
import Form.SignIn as SignInForm
import Notification exposing (Notification)
import Page
import Types exposing (..)
import UI.Column as Column

import Browser.Dom as Dom
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { signin : SignInForm.Model
  , notification : Notification
  }

initial : Model
initial =
  { signin = SignInForm.initial
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = SignInFormMsg SignInForm.Msg
  | ResetNotification

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg, Bool)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse pageInfo ResetNotification model
  in
  case msg of
    SignInFormMsg signinMsg ->
      let
        config =
          { pageInfo = pageInfo
          , finish = Page.goTo pageInfo Page.Studio
          }
        (signin, cmd) = SignInForm.updateWith config signinMsg model.signin
      in
        ( { model
          | signin = signin
          }
        , Cmd.map SignInFormMsg cmd
        , Form.isFinished signinMsg
        )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      , False
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Html.div
      [ Html.Attributes.class "columns"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-full" ]
          [ Html.map SignInFormMsg <|
            Form.viewWith "Sign In" SignInForm.view model.signin
          ]
      ]
  ] ++
  Notification.view model.notification
