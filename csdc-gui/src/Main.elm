module Main exposing (..)

import CSDC.API as API
import CSDC.Types exposing (..)
import CSDC.View.Menu as Menu
import CSDC.View.Input as CSDCInput
import CSDC.View.NewPerson as NewPerson

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
-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

noCmd : Model -> (Model, Cmd Msg)
noCmd model = (model, Cmd.none)

--------------------------------------------------------------------------------
-- Model

type alias Model =
  { inputId : Maybe (Id Person)
  , outputPerson : Maybe Person
  , newPerson : NewPerson.Model
  , view: Menu.View
  }

initial : Model
initial =
  { inputId = Nothing
  , outputPerson = Nothing
  , newPerson = NewPerson.initial
  , view = Menu.NewPerson
  }

init : () -> (Model, Cmd Msg)
init _ = noCmd initial

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputId String
  | Get
  | NewPersonMsg NewPerson.Msg
  | APIMsg API.Msg
  | MenuMsg Menu.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputId s ->
      noCmd { model | inputId = idFromString s }

    NewPersonMsg m ->
      let
        (newPerson, cmd) = NewPerson.update m model.newPerson
      in
        ( { model | newPerson = newPerson }
        , Cmd.map NewPersonMsg cmd
        )

    Get ->
      case model.inputId of
        Nothing -> noCmd model
        Just id -> (model, Cmd.map APIMsg (API.selectPerson id))

    MenuMsg menumsg ->
      case menumsg of
        Menu.SetView new ->
          noCmd { model | view = new }

    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson result ->
          case result of
            Err e ->
              noCmd model
            Ok person ->
              noCmd { model | outputPerson = Just person }

        _ ->
          noCmd model

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
  Element.map MenuMsg (Menu.view { view = model.view })

mainPanel : Model -> Element Msg
mainPanel model =
  column
    [ height fill, width <| fillPortion 5, spacing 10 ] <|
    case model.view of
      Menu.NewUnit ->
        [ column [ width <| fillPortion 2, padding 10, spacing 10 ]
          [ Input.text
              []
              { onChange = InputId
              , placeholder = Nothing
              , label = Input.labelAbove [] (text "ID")
              , text = withDefault "" (Maybe.map idToString model.inputId)
              }
          , CSDCInput.button Get "Get"
          , text <| (withDefault (Person "Nothing") model.outputPerson).name
          ]
        ]
      Menu.NewPerson ->
        [ Element.map NewPersonMsg <| NewPerson.view model.newPerson
        ]
      Menu.Explorer ->
        []
