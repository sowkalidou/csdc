module CSDC.Component.Explorer exposing
  ( Model
  , initial
  , Msg
  , update
  , view
  )

import CSDC.API as API
import CSDC.Component.Panel as Panel
import CSDC.Types exposing (..)

import Dict
import Element exposing (..)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { left : Panel.Model (Id Unit)
  , center : Panel.Model (Id Unit)
  , right : Panel.Model (Id Unit)
  }

initial : () -> (Model, Cmd Msg)
initial _ =
  ( { left = Panel.initial "Parents"
    , center = Panel.initial "Units"
    , right = Panel.initial "Children"
    }
  , Cmd.map (APIMsg Root) <| API.rootUnit
  )

--------------------------------------------------------------------------------
-- Update

type Component
  = Left
  | Center
  | Right
  | Root

type Msg
  = LeftMsg (Panel.Msg (Id Unit))
  | CenterMsg (Panel.Msg (Id Unit))
  | RightMsg (Panel.Msg (Id Unit))
  | APIMsg Component API.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
    -- XXX: use component
    APIMsg component m ->
      case m of
        API.RootUnit res ->
          case res of
            -- XXX: report error
            Err err ->
               ( model
               , Cmd.none
               )

            Ok id ->
               ( model
               , Cmd.map (APIMsg Root) <| API.selectUnit id
               )

        -- XXX: Show unit in placeholder
        API.SelectUnit id res ->
          case res of
            -- XXX: report error
            Err err ->
               ( model
               , Cmd.none
               )

            Ok unit ->
              case component of
                Root ->
                  let
                    dict = [(id,unit.name)]
                    center = Panel.update (Panel.SetItems dict) model.center
                  in
                    ( { model | center = center }
                    , Cmd.none
                    )
                -- XXX: manage other components
                _ ->
                  ( model
                  , Cmd.none
                  )
        _ ->
          ( model
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
