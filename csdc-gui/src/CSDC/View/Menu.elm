module CSDC.View.Menu exposing (..)

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

type View
  = NewPerson
  | NewUnit
  | Explorer

type alias Model =
  { view : View
  }

type Msg = SetView View

item : Model -> String -> View -> Element Msg
item model name this =
  row
    [ if model.view == this then
        Background.color <| rgb255 142 151 164
      else
        Background.color <| rgb255 92 99 118
    , if model.view == this then
        Font.bold
      else
        Font.regular
    , width fill
    , onClick <| SetView this
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
    ]

