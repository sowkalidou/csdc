module Main exposing (..)

import CSDC.API as API
import CSDC.Types exposing (..)
import CSDC.View.Menu as Menu
import CSDC.View.Input as CSDCInput

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
  , inputPerson : Maybe Person
  , outputId : Maybe (Id Person)
  , view: Menu.View
  }

initial : Model
initial =
  { inputId = Nothing
  , outputPerson = Nothing
  , inputPerson = Nothing
  , outputId = Nothing
  , view = Menu.NewPerson
  }

init : () -> (Model, Cmd Msg)
init _ = noCmd initial

--------------------------------------------------------------------------------
-- Update

type Msg
  = InputId String
  | InputPerson String
  | Get
  | Post
  | APIMsg API.Msg
  | MenuMsg Menu.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputId s ->
      noCmd { model | inputId = idFromString s }

    InputPerson s ->
      let
        ms =
         if String.isEmpty s
         then Nothing
         else Just (Person s)
      in
        noCmd { model | inputPerson = ms }

    Get ->
      case model.inputId of
        Nothing -> noCmd model
        Just id -> (model, Cmd.map APIMsg (API.selectPerson id))

    Post ->
      case model.inputPerson of
        Nothing -> noCmd model
        Just person -> (model, Cmd.map APIMsg (API.insertPerson person))

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

        API.InsertPerson result ->
          case result of
            Err e ->
              noCmd model
            Ok id ->
              noCmd { model | outputId = Just id }

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
      Menu.NewPerson ->
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
      Menu.NewUnit ->
        [ column [ width <| fillPortion 2, padding 10, spacing 10 ]
          [ Input.text
              []
              { onChange = InputPerson
              , placeholder = Nothing
              , label = Input.labelAbove [] (text "Person")
              , text = (withDefault (Person "") model.inputPerson).name
              }
          , CSDCInput.button Post "Post"
          , text <| withDefault "" <| Maybe.map idToString model.outputId
          ]
        ]
      Menu.Explorer ->
        []
