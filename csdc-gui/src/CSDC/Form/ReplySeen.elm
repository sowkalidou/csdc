module CSDC.Form.ReplySeen exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input exposing (button)
import CSDC.Form.Reply exposing (viewMessage)
import CSDC.API as API
import CSDC.Notification as Notification exposing (Notification)
import Form

import Html exposing (Html)
import Html.Attributes
import Html.Events

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { notification : Notification
  }

initial : Model
initial =
  { notification = Notification.Empty
  }

--------------------------------------------------------------------------------
-- Update

type alias Config =
  { request : Cmd (API.Response ())
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update <| Form.statelessConfig initial
  { request = config.request
  , finish = \_ -> config.finish
  }

type alias Msg = Form.Msg () () ()

--------------------------------------------------------------------------------
-- View

view : ReplyInfo a -> Model -> List (Html Msg)
view msg _ =
  [ Html.p []
      [ Html.text <| case msg.message.mtype of
          Invitation -> "Your invitation was "
          Submission -> "Your submission was "
      , Html.strong []
          [ Html.text <| case msg.rtype of
              Accept -> "accepted."
              Reject -> "rejected."
          ]
      ]
  , Html.p [] [ Html.text msg.text ]
  , Html.hr [] []
  , Html.h4 [] [ Html.text "Original Message" ]
  , Html.div [] (viewMessage msg.message)
  , Html.hr [] []
  , Html.button
      [ Html.Attributes.class "button is-primary is-pulled-right"
      , Html.Events.onClick (Form.Submit ())
      ]
      [ Html.text "Mark as seen"
      ]
  ]
