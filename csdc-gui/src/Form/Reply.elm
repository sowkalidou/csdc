module Form.Reply exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  , viewMessage
  )

import Notification exposing (Notification)
import Types exposing (..)
import Input as Input
import API as API
import Form
import Field exposing (Field)
import Page

import Html exposing (Html)
import Html.Attributes

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { reason: Field String String
  , notification : Notification
  }

initial : Model
initial =
  { reason = Field.requiredString "Reply"
  , notification = Notification.Empty
  }

reload : Model -> Model
reload model =
  { model | reason = Field.reload model.reason }

parse : ReplyType -> Model -> Maybe (ReplyType, String)
parse rtype model = Result.toMaybe <|
  Field.with model.reason <| \reason ->
    Ok (rtype, reason)

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { pageInfo : Page.Info
  , request : (ReplyType, String) -> Cmd (API.Response ())
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update
  { pageInfo = config.pageInfo
  , initial = initial
  , update = update
  , reload = reload
  , parse = parse
  , request = config.request
  , finish = \_ -> config.finish
  }

type ModelMsg
  = SetReason String

type alias Msg = Form.Msg ModelMsg ReplyType ()

update : ModelMsg -> Model -> (Model, Cmd ModelMsg)
update msg model =
  case msg of
    SetReason val ->
      ( { model | reason = Field.set val model.reason }
      , Cmd.none
      )

view : MessageInfo a -> Model -> List (Html Msg)
view msg model =
  [ Html.div [] (viewMessage msg)

  , Html.hr [] []

  , Html.p [] [ Html.i [] [ Html.text "Please reply below." ] ]

  , Input.textarea model.reason SetReason

  , Html.div
      [ Html.Attributes.class "field is-grouped is-grouped-right"
      ]
      [ Html.p
          [ Html.Attributes.class "control" ]
          [ Input.buttonDanger "Reject" Reject ]
      , Html.p
          [ Html.Attributes.class "control" ]
          [ Input.button "Accept" Accept ]
      ]
  ]

viewMessage : MessageInfo a -> List (Html msg)
viewMessage msg =
  [ Html.p [] <|
      case msg.messageType of
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

