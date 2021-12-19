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
import Form.SignUp as SignUpForm
import Notification exposing (Notification)
import Page
import Types exposing (..)
import UI.Column as Column
import UI.Tabs as Tabs

import Browser.Dom as Dom
import File exposing (File)
import File.Select
import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type Tab = SignIn | SignUp

type alias Model =
  { tab : Tab
  , signIn : SignInForm.Model
  , signUp : SignUpForm.Model
  , notification : Notification
  }

initial : Model
initial =
  { tab = SignIn
  , signIn = SignInForm.initial
  , signUp = SignUpForm.initial
  , notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = SignInFormMsg SignInForm.Msg
  | SignUpFormMsg SignUpForm.Msg
  | SetTab Tab
  | ResetNotification

update : Page.Info -> Msg -> Model -> (Model, Cmd Msg, Bool)
update pageInfo msg model =
  let
    onSuccess = Notification.withResponse pageInfo ResetNotification model
  in
  case msg of
    SetTab tab ->
      ( { model | tab = tab }
      , Cmd.none
      , False
      )

    SignInFormMsg signInMsg ->
      let
        config =
          { pageInfo = pageInfo
          , finish = Page.goTo pageInfo Page.Studio
          }
        (signIn, cmd) = SignInForm.updateWith config signInMsg model.signIn
      in
        ( { model
          | signIn = signIn
          }
        , Cmd.map SignInFormMsg cmd
        , Form.isFinished signInMsg
        )

    SignUpFormMsg signUpMsg ->
      let
        config =
          { pageInfo = pageInfo
          , finish = Cmd.none
          }
        (signUp, cmd) = SignUpForm.updateWith config signUpMsg model.signUp
      in
        ( { model
          | signUp = signUp
          , tab = if Form.isFinished signUpMsg then SignIn else model.tab
          }
        , Cmd.map SignUpFormMsg cmd
        , False
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
      [ Html.Attributes.class "columns is-vcentered"
      , Html.Attributes.style "height" "100%"
      ]
      [ Html.div
          [ Html.Attributes.class "column is-full"
          ]
          [ Html.div
              [ Html.Attributes.class "is-flex is-justify-content-center"
              , Html.Attributes.style "margin-bottom" "30px"
              ]
              [ Html.map SetTab <|
                Tabs.view (\tab -> tab == model.tab)
                  [ (SignIn, "Sign In")
                  , (SignUp, "Sign Up")
                  ]
              ]
          , case model.tab of
              SignIn ->
                Html.map SignInFormMsg <|
                Form.viewWith "Sign In" SignInForm.view model.signIn
              SignUp ->
                Html.map SignUpFormMsg <|
                Form.viewWith "Sign Up" SignUpForm.view model.signUp
          ]
      ]
  ] ++
  Notification.view model.notification
