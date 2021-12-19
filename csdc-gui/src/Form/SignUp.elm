module Form.SignUp exposing
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
  { name : Field String String
  , email : Field String String
  , password : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , email = Field.requiredString "Email"
  , password = Field.requiredString "Password"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | name = Field.reload model.name
  , email = Field.reload model.email
  , password = Field.reload model.password
  }

parse : Model -> Maybe NewUser
parse model = Result.toMaybe <|
  Field.with model.name <| \name ->
  Field.with model.email <| \email ->
  Field.with model.password <| \password ->
  Ok
    { name = name
    , email = email
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
  , request = API.signup
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetName String
  | SetEmail String
  | SetPassword String

type alias Msg = Form.Msg ModelMsg () (Id Person)

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetName val ->
      ( { model | name = Field.set val model.name }
      , Cmd.none
      )
    SetEmail val ->
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
  [ Input.text model.name SetName
  , Input.email model.email SetEmail
  , Input.password model.password SetPassword
  , Input.button "Sign Up" ()
  ]
