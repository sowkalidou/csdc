module CSDC.Form.UnitDelete exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import CSDC.Component.Preview as Preview
import CSDC.Types exposing (..)
import CSDC.Input as Input
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

view : Model -> List (Html Msg)
view _ =
  [ Html.p
      []
      [ Html.text "Are you sure you want to delete this unit?" ]
  , Html.p
      []
      [ Html.text "This operation is not reversible." ]
  , Input.buttonDanger "Delete" ()
  ]
