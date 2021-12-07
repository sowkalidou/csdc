module Form.Thread exposing
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

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { subject : Field String String
  , text : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { subject = Field.requiredString "Subject"
  , text = Field.requiredString "Text"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | subject = Field.reload model.subject
  , text = Field.reload model.text
  }

parse : Model -> Maybe NewThread
parse model = Result.toMaybe <|
  Field.with model.subject <| \subject ->
  Field.with model.text <| \text ->
  Ok
    { subject = subject
    , text = text
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { id : Id Unit
  , finish : Id Thread -> Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.createUnitThread config.id
  , finish = config.finish
  }

type ModelMsg
  = SetSubject String
  | SetText String

type alias Msg = Form.Msg ModelMsg () (Id Thread)

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetSubject val ->
      ( { model | subject = Field.set val model.subject }
      , Cmd.none
      )
    SetText val ->
      ( { model | text = Field.set val model.text }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.text model.subject SetSubject
  , Input.textarea model.text SetText
  , Input.button "Create" ()
  ]
