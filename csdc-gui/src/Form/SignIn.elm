module Form.SignIn exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import API as API
import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import Form
import Page

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { email : Field String String
  , password : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { email = Field.email "Email"
  , password = Field.password "Password"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | email = Field.reload model.email
  , password = Field.reload model.password
  }

parse : Model -> Maybe Login
parse model = Result.toMaybe <|
  Field.with model.email <| \email ->
  Field.with model.password <| \password ->
  Ok
    { email = email
    , password = password
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { pageInfo : Page.Info
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.signin
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetName String
  | SetPassword String

type alias Msg = Form.Msg ModelMsg () ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetName val ->
      ( { model | email = Field.set val model.email }
      , Cmd.none
      )
    SetPassword val ->
      ( { model | password = Field.set val model.password }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.text model.email SetName
  , Input.password model.password SetPassword
  , Input.button "Sign In" ()
  ]
