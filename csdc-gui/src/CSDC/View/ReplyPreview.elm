module CSDC.View.ReplyPreview exposing
  ( Model
  , initial
  , Msg (..)
  , updateMember
  , updateSubpart
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)
import CSDC.View.MessagePreview exposing (viewMessage)
import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)

import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Model =
  { notification : Notification
  }

initial : Model
initial =
  { notification = Notification.Empty
  }

type Msg
  = MarkAsSeen
  | ResetNotification
  | APIMsg API.Msg

updateMember : Id (Reply Member) -> Msg -> Model -> (Model, Cmd Msg)
updateMember id = updateWith <|
  Cmd.map APIMsg (API.viewReplyMember id)

updateSubpart : Id (Reply Subpart) -> Msg -> Model -> (Model, Cmd Msg)
updateSubpart id = updateWith <|
  Cmd.map APIMsg (API.viewReplySubpart id)

updateWith : Cmd Msg -> Msg -> Model -> (Model, Cmd Msg)
updateWith cmd msg model =
  case msg of
    MarkAsSeen ->
      ( { model | notification = Notification.Processing }
      , cmd
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
        API.ViewReplyMember response -> onSuccess response <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.none
          )

        API.ViewReplySubpart response -> onSuccess response <| \_ ->
          ( { initial | notification = Notification.Success }
          , Cmd.none
          )

        _ ->
          (model, Cmd.none)

view : ReplyInfo a -> Html Msg
view msg = Preview.make <|
  [ Html.h4 []
      [ Html.text <|
        ( case msg.mtype of
            Invitation -> "Invitation"
            Submission -> "Submission"
        ) ++ " " ++
        ( case msg.rtype of
            Accept -> "Accepted"
            Reject -> "Rejected"
        )
      ]
  ] ++
  [ Html.p
      []
      [ Html.text msg.text
      , Html.hr [] []
      ]
  , Html.h4 [] [ Html.text "Original Message" ]
  ] ++
  viewMessage msg.message ++
  [ Html.hr [] []
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick MarkAsSeen
      ]
      [ Html.text "Mark as seen"
      ]
  ]
