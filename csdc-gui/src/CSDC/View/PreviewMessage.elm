module CSDC.View.PreviewMessage exposing
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
  { reason = Field.requiredString "Answer"
  , notification = Notification.Empty
  }

type Msg a
  = SetReason String
  | Reply -- (Reply a)
  | Reset
  | ResetNotification

update : Msg a -> Model -> (Model, Cmd (Msg a))
update msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )

    Reply ->
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

view : MessageInfo a -> Model -> Html (Msg a)
view (MessageInfo msg) model = Preview.make <|
  [ Html.h4 []
      [ Html.text <|
        case msg.mtype of
          Invitation -> "Invitation"
          Submission -> "Submission"
      ]

  , Html.p [] <|
      case msg.mtype of
        Invitation ->
          [ Html.strong [] [ Html.text msg.right ]
          , Html.text " is inviting "
          , Html.strong [] [ Html.text msg.left ]
          , Html.text " to join the unit."
          ]
        Submission ->
          [ Html.strong [] [ Html.text msg.left ]
          , Html.text " would like to join "
          , Html.strong [] [ Html.text msg.right ]
          , Html.text "."
          ]

  , Html.p [] [ Html.text msg.text ]

  , Html.hr [] []

  , Html.p [] [ Html.i [] [ Html.text "Please answer below." ] ]

  , Input.textarea model.reason SetReason

  , Html.div
      [ Html.Attributes.class "field is-grouped is-grouped-right"
      ]
      [ Html.p [ Html.Attributes.class "control" ] [ Input.buttonDanger Reply "Refuse" ]
      , Html.p [ Html.Attributes.class "control" ] [ Input.button Reply "Accept" ]
      ]
  ]
