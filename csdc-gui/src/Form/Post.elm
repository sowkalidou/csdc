module Form.Post exposing
  ( Model
  , initial
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
  { text : Field String String
  , notification : Notification
  }

initial : Model
initial =
  { text = Field.requiredString "Text"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | text = Field.reload model.text
  }

parse : Model -> Maybe NewPost
parse model = Result.toMaybe <|
  Field.with model.text <| \text ->
  Ok
    { text = text
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { id : Id Thread
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = \_ -> parse
  , request = API.createThreadPost config.id
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetText String

type alias Msg = Form.Msg ModelMsg () (Id Post)

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetText val ->
      ( { model | text = Field.set val model.text }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view model =
  [ Input.textarea model.text SetText
  , Input.button "Create" ()
  ]
