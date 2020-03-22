module Main exposing (..)

import CSDC.Types exposing (..)
import CSDC.API as API
import CSDC.View.Menu as Menu

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
  , inputPerson : Maybe Person
  , outputId : Maybe (Id Person)
  , view: Menu.View
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing Nothing Nothing Nothing Menu.NewPerson
  , Cmd.none
  )

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
      ({ model | inputId = idFromString s }, Cmd.none)

    InputPerson s ->
      let
        ms =
         if String.isEmpty s
         then Nothing
         else Just (Person s)
      in
        ({ model | inputPerson = ms }, Cmd.none)

    Get ->
      case model.inputId of
        Nothing -> (model, Cmd.none)
        Just id -> (model, Cmd.map APIMsg (API.selectPerson id))

    Post ->
      case model.inputPerson of
        Nothing -> (model, Cmd.none)
        Just person -> (model, Cmd.map APIMsg (API.insertPerson person))

    MenuMsg menumsg ->
      case menumsg of
        Menu.SetView new ->
          ({ model | view = new }, Cmd.none)

    APIMsg apimsg ->
      case apimsg of
        API.SelectPerson result ->
          case result of
            Err e -> (model, Cmd.none)
            Ok person ->
              ({ model | outputPerson = Just person }, Cmd.none)

        API.InsertPerson result ->
          case result of
            Err e -> (model, Cmd.none)
            Ok id ->
              ({ model | outputId = Just id }, Cmd.none)

        _ -> (model, Cmd.none)

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
    [ height fill, width <| fillPortion 5 ]
    [ column [ width <| fillPortion 2 ]
      [ Input.text
          []
          { onChange = InputId
          , placeholder = Nothing
          , label = Input.labelAbove [] (text "ID")
          , text = withDefault "" (Maybe.map idToString model.inputId)
          }
      , Input.button
          []
          { onPress = Just Get
          , label = text "Get"
          }
      , text <| (withDefault (Person "Nothing") model.outputPerson).name
      ]
    , column [ width <| fillPortion 2 ]
      [ Input.text
          []
          { onChange = InputPerson
          , placeholder = Nothing
          , label = Input.labelAbove [] (text "Person")
          , text = (withDefault (Person "") model.inputPerson).name
          }
      , Input.button
          []
          { onPress = Just Post
          , label = text "Post"
          }
      , text <| withDefault "" <| Maybe.map idToString model.outputId
      ]
   ]
