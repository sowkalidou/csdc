module CSDC.Form.SubmissionMember exposing
  ( Model
  , initial
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

import Html exposing (Html)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { reason: Field String String
  , notification : Notification
  }

initial : Model
initial =
  { reason = Field.requiredString "Message"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model
  | reason = Field.reload model.reason
  }

parse : NewMember -> Model -> Maybe (NewMessage NewMember)
parse member model = Result.toMaybe <|
  Field.with model.reason <| \reason ->
  Ok
    { mtype = Submission
    , text = reason
    , value = member
    }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { initial = initial
  , update = update
  , reload = reload
  , parse = parse
  , request = API.sendMessageMember
  , finish = \_ -> config.finish
  }

type alias Msg = Form.Msg ModelMsg NewMember (Id (Message NewMember))

type ModelMsg
  = SetReason String

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : NewMember -> Model -> List (Html Msg)
view member model =
  [ Html.p [] [ Html.i [] [ Html.text "Please write a message below." ] ]
  , Input.textarea model.reason SetReason
  , Input.button "Send" member
  ]
