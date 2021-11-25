module CSDC.View.MessageCreate exposing
  ( Model
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import Field exposing (Field)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Model =
  { reason: Field String String
  , notification : Notification
  }

initial : Model
initial =
  { reason = Field.requiredString "Reason"
  , notification = Notification.Empty
  }

type Msg a
  = SetReason String
  | Create
  | Reset
  | ResetNotification

update : Msg a -> Model -> (Model, Cmd (Msg a))
update msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )

    Create ->
      ( model
      , Cmd.none
      )

    Reset ->
      ( initial
      , Cmd.none
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

view : MessageType -> Model -> Html (Msg a)
view mtype model = Preview.make <|
  [ Html.h4 []
      [ Html.text <|
        case mtype of
          Invitation -> "Send Invitation"
          Submission -> "Send Submission"
      ]

  , Html.p [] [ Html.i [] [ Html.text "Please write a message below." ] ]

  , Input.textarea model.reason SetReason

  , Html.p [ Html.Attributes.class "control" ] [ Input.button Create "Send" ]
  ]
