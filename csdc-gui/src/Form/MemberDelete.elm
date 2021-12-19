module Form.MemberDelete exposing
  ( Model
  , initial
  , Msg
  , updateWith
  , Who (..)
  , view
  )

import Types exposing (Id, Member)
import Input exposing (buttonDanger)
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
  , member : Id Member
  , finish : Cmd Msg
  }

updateWith : Config -> Msg -> Model -> (Model, Cmd Msg)
updateWith config = Form.update <| Form.statelessConfig initial
  { pageInfo = config.pageInfo
  , request = API.deleteMember config.member
  , finish = \_ -> config.finish
  }

type alias Msg = Form.Msg () () ()

--------------------------------------------------------------------------------
-- View

type Who = Unit | Person

view : Who -> Model -> List (Html Msg)
view who _ =
  [ Html.p []
      [ Html.text <|
        case who of
          Unit -> "Are you sure you want to remove this person from the unit?"
          Person -> "Are you sure you want to leave this unit?"
      ]
  , buttonDanger "Confirm" ()
  ]
