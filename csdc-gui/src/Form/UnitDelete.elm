module Form.UnitDelete exposing
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
import Page

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
  { pageInfo : Page.Info
  , request : Cmd (API.Response ())
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update <| Form.statelessConfig initial
  { pageInfo = config.pageInfo
  , request = config.request
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
