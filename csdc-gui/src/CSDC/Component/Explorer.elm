module CSDC.Component.Explorer exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.Component.Panel as Panel

import Dict
import Element exposing (..)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { left : Panel.Model
  , center : Panel.Model
  , right : Panel.Model
  }

initial : Model
initial =
  { left = Panel.initial "Parents"
  , center = Panel.initial "Units"
  , right = Panel.initial "Children"
  }

--------------------------------------------------------------------------------
-- Update

type Msg
  = InitRequest
  | InitResponse
  | LeftMsg Panel.Msg
  | CenterMsg Panel.Msg
  | RightMsg Panel.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InitRequest ->
      ( model
      , Cmd.none
      )
    InitResponse ->
      ( model
      , Cmd.none
      )
    LeftMsg m ->
      ( { model | left = Panel.update m model.left }
      , Cmd.none
      )
    CenterMsg m ->
      ( { model | center = Panel.update m model.center }
      , Cmd.none
      )
    RightMsg m ->
      ( { model | right = Panel.update m model.right }
      , Cmd.none
      )

--------------------------------------------------------------------------------
-- View

view : Model -> List (Element Msg)
view model =
  [ row
      [ height <| fillPortion 3
      , width fill
      , spacing 10
      ]
      [ map LeftMsg <| Panel.view model.left
      , map CenterMsg <| Panel.view model.center
      , map RightMsg <| Panel.view model.right
      ]
  , row
      [ height <| fillPortion 1
      , width fill
      ]
      [ text "Placeholder" ]
  ]
