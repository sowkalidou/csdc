module CSDC.Form.Unit exposing
  ( Model
  , initial
  , fromUnit
  , Msg
  , updateWith
  , view
  )

import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)
import Form
import Validation

import Html exposing (Html)
import Html.Attributes
import Html.Events

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
parse model =
  let
    result =
      Validation.andThen (Field.validate model.name) <| \name ->
      Validation.andThen (Field.validate model.description) <| \description ->
      Validation.valid
        { name = name
        , description = description
        }
  in
    case Validation.validate result of
      Err _ -> Nothing
      Ok unit -> Just unit

--------------------------------------------------------------------------------
-- Update

type alias Config a =
  { request : NewUnit -> Cmd (API.Response a)
  , finish : a -> Cmd (Msg a)
  }

updateWith : Config a -> Msg a -> Model -> (Model, Cmd (Msg a))
updateWith config = Form.update
  { initial = initial
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
