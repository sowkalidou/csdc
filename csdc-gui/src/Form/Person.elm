module Form.Person exposing
  ( Model
  , initial
  , fromPerson
  , Msg
  , updateWith
  , view
  )

import API as API
import Notification as Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import Field exposing (Field)
import Form

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { name : Field String String
  , description : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , description = Field.requiredString "Description"
  , notification = Notification.Empty
  }

fromPerson : Person -> Model
fromPerson person =
  { initial
  | name = Field.set person.name initial.name
  , description = Field.set person.description initial.description
  }

reload : Model -> Model
reload model =
  { model
  | name = Field.reload model.name
  , description = Field.reload model.description
  }

parse : Model -> Maybe PersonUpdate
parse model = Result.toMaybe <|
  Field.with model.name <| \name ->
  Field.with model.description <| \description ->
  Ok
    { name = name
    , description = description
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { id : Id Person
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.updatePerson config.id
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetName String
  | SetDescription String

type alias Msg = Form.Msg ModelMsg () ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetName val ->
      ( { model | name = Field.set val model.name }
      , Cmd.none
      )
    SetDescription val ->
      ( { model | description = Field.set val model.description }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.text model.name SetName
  , Input.textarea model.description SetDescription
  , Input.button "Save" ()
  ]
