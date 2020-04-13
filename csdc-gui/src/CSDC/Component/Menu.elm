module CSDC.Component.Menu exposing
  ( Model (..)
  , initial
  , Msg (..)
  , update
  , view
  )

import CSDC.Types exposing (..)
import CSDC.API as API

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe
import Maybe exposing (withDefault)
import String

--------------------------------------------------------------------------------
-- Model

type Model
  = NewPerson
  | NewUnit
  | Explorer
  | Studio

initial : Model
initial = NewPerson

--------------------------------------------------------------------------------
-- Update

type Msg = SetModel Model

update : Msg -> Model -> Model
update (SetModel model) _ = model

--------------------------------------------------------------------------------
-- View

item : Model -> String -> Model -> Element Msg
item model name this =
  row
    [ if model == this then
        Background.color <| rgb255 142 151 164
      else
        Background.color <| rgb255 92 99 118
    , if model == this then
        Font.bold
      else
        Font.regular
    , width fill
    , onClick <| SetModel this
    , padding 10
    ]
    [ el [ centerX ] (text name)
    ]

title : Element msg
title =
  row
    [ Background.color <| rgb255 42 44 52
    , Font.bold
    , Font.size 20
    , Font.color <| rgb255 243 243 244
    , width fill
    , padding 20
    ]
    [ el [ centerX ] (text "CS-DC Network")
    ]

view : Model -> Element Msg
view model =
  column
    [ height fill
    , width <| fillPortion 1
    , Background.color <| rgb255 92 99 118
    , Font.color <| rgb255 255 255 255
    ]
    [ title
    , item model "New Person" NewPerson
    , item model "New Unit" NewUnit
    , item model "Explorer" Explorer
    , item model "Studio" Studio
    ]

