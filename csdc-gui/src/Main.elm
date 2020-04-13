module Main exposing (..)

import CSDC.API as API
import CSDC.Component.Menu as Menu
import CSDC.Component.NewPerson as NewPerson
import CSDC.Component.NewUnit as NewUnit
import CSDC.Types exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Maybe
import Maybe exposing (withDefault)
import String

--------------------------------------------------------------------------------
-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { inputId : Maybe (Id Person)
  , outputPerson : Maybe Person
  , newPerson : NewPerson.Model
  , newUnit : NewUnit.Model
  , menu: Menu.Model
  }

initial : Model
initial =
  { inputId = Nothing
  , outputPerson = Nothing
  , newPerson = NewPerson.initial
  , newUnit = NewUnit.initial
  , menu = Menu.initial
  }

init : () -> (Model, Cmd Msg)
init _ = (initial, Cmd.none)

--------------------------------------------------------------------------------
-- Update

type Msg
  = NewPersonMsg NewPerson.Msg
  | NewUnitMsg NewUnit.Msg
  | MenuMsg Menu.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewUnitMsg m ->
      let
        (newUnit, cmd) = NewUnit.update m model.newUnit
      in
        ( { model | newUnit = newUnit }
        , Cmd.map NewUnitMsg cmd
        )

    NewPersonMsg m ->
      let
        (newPerson, cmd) = NewPerson.update m model.newPerson
      in
        ( { model | newPerson = newPerson }
        , Cmd.map NewPersonMsg cmd
        )

    MenuMsg m ->
      let
        menu = Menu.update m model.menu
      in
        ( { model | menu = menu }
        , Cmd.none
        )

--------------------------------------------------------------------------------
-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--------------------------------------------------------------------------------
-- View

view : Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
        [ menuPanel model
        , mainPanel model
        ]

menuPanel : Model -> Element Msg
menuPanel model =
  Element.map MenuMsg <| Menu.view model.menu

mainPanel : Model -> Element Msg
mainPanel model =
  column
    [ height fill, width <| fillPortion 5, spacing 10 ] <|
    case model.menu of
      Menu.NewPerson ->
        [ Element.map NewPersonMsg <| NewPerson.view model.newPerson
        ]

      Menu.NewUnit ->
        [ Element.map NewUnitMsg <| NewUnit.view model.newUnit
        ]

      Menu.Explorer ->
        []

      Menu.Studio ->
        []
