module Form.Unit exposing
  ( Model
  , initial
  , fromUnit
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
  , description : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { name = Field.requiredString "Name"
  , description = Field.requiredString "Description"
  , notification = Notification.Empty
  }

fromUnit : Unit -> Model
fromUnit unit =
  { initial
  | name = Field.set unit.name initial.name
  , description = Field.set unit.description initial.description
  }

reload : Model -> Model
reload model =
  { model
  | name = Field.reload model.name
  , description = Field.reload model.description
  }

-- Also unit update...
parse : Model -> Maybe NewUnit
parse model = Result.toMaybe <|
  Field.with model.name <| \name ->
  Field.with model.description <| \description ->
  Ok
    { name = name
    , description = description
    }

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { pageInfo : Page.Info
  , request : NewUnit -> Cmd (API.Response a)
  , finish : a -> Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = config.request
  , finish = config.finish
  }

type alias Msg a = Form.Msg ModelMsg () a

type ModelMsg
  = SetName String
  | SetDescription String

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

view : Model -> List (Html (Msg a))
view model =
  [ Input.text model.name SetName
  , Input.textarea model.description SetDescription
  , Input.button "Save" ()
  ]
