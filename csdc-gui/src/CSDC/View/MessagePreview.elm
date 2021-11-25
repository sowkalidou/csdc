module CSDC.View.MessagePreview exposing
  ( Model
  , initial
  , Msg (..)
  , updateMember
  , updateSubpart
  , view
  , viewMessage
  )

import CSDC.Component.Preview as Preview
import CSDC.Notification as Notification exposing (Notification)
import CSDC.Types exposing (..)
import CSDC.Input as Input
import CSDC.API as API
import Field exposing (Field)
import Validation

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

type Msg
  = SetReason String
  | Reply ReplyType
  | Reset
  | ResetNotification
  | APIMsg API.Msg

updateMember : Id (Message Member) -> Msg -> Model -> (Model, Cmd Msg)
updateMember mid = updateWith <| \reason -> \rtype ->
  Cmd.map APIMsg <| API.sendReplyMember
    { rtype = rtype
    , text = reason
    , message = mid
    }

updateSubpart : Id (Message Subpart) -> Msg -> Model -> (Model, Cmd Msg)
updateSubpart mid = updateWith <| \reason -> \rtype ->
  Cmd.map APIMsg <| API.sendReplySubpart
    { rtype = rtype
    , text = reason
    , message = mid
    }

updateWith : (String -> ReplyType -> Cmd Msg) -> Msg -> Model -> (Model, Cmd Msg)
updateWith makeCmd msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )

    Reply rtype ->
      case Validation.validate (Field.validate model.reason) of
        Err e ->
          ( { model | notification = Notification.Error e }
          , Cmd.none
          )
        Ok reason ->
          ( { model | notification = Notification.Processing }
          , makeCmd reason rtype
          )

    Reset ->
      ( initial
      , Cmd.none
      )

    ResetNotification ->
      ( { model | notification = Notification.Empty }
      , Cmd.none
      )

    APIMsg apiMsg ->
      let
        onSuccess = Notification.withResponse ResetNotification model
      in
      case apiMsg of
        API.SendReplyMember response -> onSuccess response <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.none
          )

        API.SendReplySubpart response -> onSuccess response <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

view : MessageInfo a -> Model -> Html Msg
view msg model = Preview.make <|
  [ Html.h4 []
      [ Html.text <|
        case msg.mtype of
          Invitation -> "Invitation"
          Submission -> "Submission"
      ]
  ] ++
  viewMessage msg ++
  [ Html.hr [] []

  , Html.p [] [ Html.i [] [ Html.text "Please answer below." ] ]

  , Input.textarea model.reason SetReason

  , Html.div
      [ Html.Attributes.class "field is-grouped is-grouped-right"
      ]
      [ Html.p [ Html.Attributes.class "control" ] [ Input.buttonDanger (Reply Reject) "Reject" ]
      , Html.p [ Html.Attributes.class "control" ] [ Input.button (Reply Accept) "Accept" ]
      ]
  ]

viewMessage : MessageInfo a -> List (Html msg)
viewMessage msg =
  [ Html.p [] <|
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
  ]

