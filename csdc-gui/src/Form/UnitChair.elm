module Form.UnitChair exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , view
  )

import Types exposing (..)
import Input as Input
import API as API
import Notification exposing (Notification)
import Form

import Html exposing (Html)

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
  { unit : Id Unit
  , person : Id Person
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update <| Form.statelessConfig initial
  { request = API.updateUnitChair config.unit config.person
  , finish = \_ -> config.finish
  }

type alias Msg = Form.Msg () () ()

--------------------------------------------------------------------------------
-- View

view : Model -> List (Html Msg)
view _ =
  [ Html.p
      []
      [ Html.text "Are you sure you want to make this person the chair of the unit?" ]
  , Input.buttonDanger "Change chair" ()
  ]
