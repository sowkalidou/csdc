module CSDC.Component.Menu exposing
  ( Model (..)
  , initial
  , toPage
  , fromPage
  , Msg (..)
  , view
  )

import CSDC.Types exposing (..)
import CSDC.API as API
import CSDC.Page as Page

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
  = Explorer
  | Studio
  | Admin

initial : Model
initial = Studio

toPage : Model -> Page.Page
toPage model =
  case model of
    Explorer -> Page.Explorer
    Studio -> Page.Studio
    Admin -> Page.Admin

fromPage : Page.Page -> Maybe Model
fromPage page =
  case page of
    Page.Explorer -> Just Explorer
    Page.Studio -> Just Studio
    Page.Admin -> Just Admin
    _ -> Nothing

--------------------------------------------------------------------------------
-- Update

type Msg = SetItem Model

--------------------------------------------------------------------------------
-- View

item : Maybe Model -> String -> Model -> Element Msg
item model name this =
  row
    [ if model == Just this then
        Background.color <| rgb255 142 151 164
      else
        Background.color <| rgb255 92 99 118
    , if model == Just this then
        Font.bold
      else
        Font.regular
    , width fill
    , onClick <| SetItem this
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
    [ el [ centerX ] (text "CS-DC DAO")
    ]

view : Maybe Model -> Element Msg
view model =
  column
    [ height fill
    , width <| fillPortion 1
    , Background.color <| rgb255 92 99 118
    , Font.color <| rgb255 255 255 255
    ]
    [ title
    , item model "Studio" Studio
    , item model "Explorer" Explorer
    , item model "Admin" Admin
    ]

